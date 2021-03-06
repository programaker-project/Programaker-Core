import { ConnectionService } from "../../connection.service";
import { CustomBlockService } from "../../custom_block.service";
import { EnvironmentService } from "../../environment.service";
import { ServiceService } from "../../service.service";
import { Session } from "../../session";
import { BridgeConnection } from "../../connection";
import { iconDataToUrl } from "../../utils";
import { ResolvedCustomBlock } from "../../custom_block";

export type BlockDef = {
    id: string,
    message: string,
    icon?: string,
};

export type CategoryDef = {
    id: string,
    name: string,
    blocks: BlockDef[],
}

export interface ISpreadsheetToolbox {
    categories: CategoryDef[];
    nonEmptyCategories: CategoryDef[];
    blockMap: { [key: string]: { cat: CategoryDef, block: ResolvedCustomBlock } };
}

export function simplify_id(block: ResolvedCustomBlock) {
    return block.service_port_id.substr(0, 2)
        + block.service_port_id.substr(block.service_port_id.length - 2)
        + '_' + block.function_name
}

export class SpreadsheetToolbox {

    public categories: CategoryDef[] = [];
    public nonEmptyCategories: CategoryDef[];
    public blockMap: { [key: string]: { cat: CategoryDef, block: ResolvedCustomBlock } } = {};

    constructor (
        private customBlockService: CustomBlockService,
        private serviceService: ServiceService,
        private environmentService: EnvironmentService,
        private programId: string,
        private connectionService: ConnectionService,
        private session: Session,
    ) {
        this.init();
    }

    private async init() {
        const availableConnectionsQuery = this.connectionService.getAvailableBridgesForNewConnectionOnProgram(this.programId);

        const [connections, services] =  await Promise.all([
            this.connectionService.getConnectionsOnProgram(this.programId),
            this.serviceService.getAvailableServicesOnProgram(this.programId),
        ]);

        const connection_by_id: {[key: string]: BridgeConnection} = {};

        for (const connection of connections) {
            connection_by_id[connection.bridge_id] = connection;
        }

        const blockMap: { [key: string]: { cat: CategoryDef, block: ResolvedCustomBlock } } = {};
        const categories: {id: string, name: string, blocks: BlockDef[]}[] = [];
        for (const service of services) {
            categories.push({ id: service.id, name: service.name, blocks: [] });
        }

        const skip_resolve_argument_options = true; // Enum options will be filled when needed
        const blocks = await this.customBlockService.getCustomBlocksOnProgram(this.programId, skip_resolve_argument_options);
        for (const block of blocks) {
            let icon: string | null = null;

            const connection = connection_by_id[block.service_port_id];
            if (connection) {
                icon = iconDataToUrl(this.environmentService, connection.icon, connection.bridge_id);
            }
            else {
                console.error("No connection found for", block, connection_by_id);
            }

            const category = categories.find(c => c.id === block.service_port_id);
            if (!category) {
                console.error("Ignoring block. Category not found for", block, block.service_port_id);
                continue;
            }

            const reducedId = simplify_id(block);
            category.blocks.push({
                icon: icon,
                message: block.message,
                id: reducedId
            });
            blockMap[reducedId] = { cat: category, block: block };
        }

        this.categories = categories;
        this.nonEmptyCategories = this.categories.filter(cat => cat.blocks.length > 0);
        this.blockMap = blockMap;
    }
}
