import { ConnectionService } from "app/connection.service";
import { CustomBlockService } from "app/custom_block.service";
import { EnvironmentService } from "app/environment.service";
import { ServiceService } from "app/service.service";
import { Session } from "app/session";
import { BridgeConnection } from "app/connection";
import { iconDataToUrl } from "app/utils";
import { ResolvedCustomBlock } from "app/custom_block";

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
    categories: CategoryDef[] = [];
    nonEmptyCategories: CategoryDef[];
    blockMap: { [key: string]: { cat: CategoryDef, block: ResolvedCustomBlock } };
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
            const reducedId = (
                block.service_port_id.substr(0, 2)
                    + block.service_port_id.substr(block.service_port_id.length - 2)
                    + '_' + block.function_name
            );
            category.blocks.push({
                icon: icon,
                message: block.message,
                id: reducedId
            });
            blockMap[reducedId] = { cat: category, block: block };
        }

        const availableBridges = await availableConnectionsQuery;
        for (const bridge of availableBridges) {
            // base.addActuator(() =>
            //     new ToolboxFlowButton({
            //         message: "Connect to " + bridge.name,
            //         action: () => {
            //             const dialogRef = dialog.open(AddConnectionDialogComponent, {
            //                 disableClose: false,
            //                 data: {
            //                     programId: programId,
            //                     bridgeInfo: bridge,
            //                 }
            //             });

            //             dialogRef.afterClosed().subscribe(async (result) => {
            //                 if (!result) {
            //                     console.log("Cancelled");
            //                     return;
            //                 }

            //                 console.debug("Reloading toolbox...");
            //                 triggerToolboxReload();
            //             });
            //         }
            //     }), bridge.id);
        }

        this.categories = categories;
        this.nonEmptyCategories = this.categories.filter(cat => cat.blocks.length > 0);
        this.blockMap = blockMap;
    }

    // function get_block_message(block: ResolvedCustomBlock): [string, number[]] {
    //     const output_indexes = get_output_indexes(block);

    //     const translationTable: number[] = [];
    //     let offset = 0;

    //     const message = block.message.replace(/%(\d+)/g, (_match, digits) => {
    //         const num = parseInt(digits);
    //         if (output_indexes.indexOf(num - 1) < 0) { // %num are 1-indexed
    //             translationTable[num] = num - offset;
    //             return `%i${num - offset}`;
    //         }
    //         else {
    //             offset += 1;
    //             if (output_indexes.length !== 1) {
    //                 console.error('TODO: Index output remapping', block);
    //             }
    //             return '%o1';
    //         }
    //     });

    //     return [message, translationTable];
    // }
}
