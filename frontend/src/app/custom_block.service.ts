import { Injectable } from '@angular/core';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';
import { CustomBlock, ResolvedCustomBlock, ResolvedBlockArgument, BlockArgument, DynamicBlockArgument, StaticBlockArgument, ResolvedDynamicBlockArgument } from './custom_block';

@Injectable()
export class CustomBlockService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getCustomBlocksUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/custom-blocks/';
    }

    async getCustomBlocks(): Promise<ResolvedCustomBlock[]> {
        const blocks = await this.getCustomBlocksUrl().then(url =>
            this.http.get(url,
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                })
                .map(response => {
                    const blocks: CustomBlock[] = [];
                    for (const service_port_id of Object.keys(response)) {
                        for (const block of response[service_port_id]) {
                            block.service_port_id = service_port_id;
                            block.id = 'services.' + service_port_id + '.' + block.block_id;

                            blocks.push(block);
                        }
                    }

                    console.log("Blocks:", blocks);
                    return blocks;
                })
                .toPromise());

        return Promise.all(blocks.map(async (block): Promise<ResolvedCustomBlock> => {
            return await this._resolveBlock(block);
        }));
    }

    private async _resolveBlock(block: CustomBlock): Promise<ResolvedCustomBlock> {
        // Note that the block is not copied, the resolution is done destructively!
        const resolvedBlock = block as ResolvedCustomBlock;
        const newArguments: ResolvedBlockArgument[] = [];
        for (const argument of block.arguments) {
            newArguments.push(await this._resolveArgument(argument, block));
        }

        resolvedBlock.arguments = newArguments;
        return resolvedBlock;
    }

    private async _resolveArgument(arg: BlockArgument, block: CustomBlock): Promise<ResolvedBlockArgument> {
        // Note that the argument is not copied, the resolution is done destructively!
        if (!(arg as DynamicBlockArgument).callback) {
            return arg as StaticBlockArgument;
        }

        const dynamicArg = arg as DynamicBlockArgument;
        const options = await this.getArgOptions(dynamicArg, block);
        const resolved = dynamicArg as ResolvedDynamicBlockArgument;
        resolved.options = options;

        return resolved;
    }

    async getArgOptions(arg: DynamicBlockArgument, block: CustomBlock): Promise<[string, string][]> {
        const userApiRoot = await this.sessionService.getApiRootForUserId();

        return (this.http.get(userApiRoot + '/bridges/id/' + block.service_port_id + '/callback/' + arg.callback,
            {
                headers: this.sessionService.getAuthHeader(),
            })
            .map((response: { result: { [key: string]: { name: string } } }) => {
                console.log("Options:", response);
                const options = [];
                const result = response.result;

                for (const key of Object.keys(result)) {
                    options.push([result[key].name, key]);
                }

                return options;
            })
            .toPromise());
    }
}
