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
    static DataCallbackCachePrefix = "plaza-data-callback";

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
        let options : [string, string][];
        try {
            options = await this.getCachedArgOptions(dynamicArg, block);
            const loading = options[0][1] === '__plaza_internal_loading';

            // Reload asynchronously
            this.getArgOptions(dynamicArg, block).then((result) => {
                if (result.length === 0){
                    throw Error("No options found for dynamic argument: " + arg);
                }

                this.cacheArgOptions(dynamicArg, block, result);

                // Replace all options in place
                let index;
                for (index = 0; index < result.length; index++) {
                    options[index] = result[index];
                }

                while (options.length > index) {
                    options.pop();
                }
            }).catch((err) => {
                console.error(err);
                if (loading) {
                    options[0] = ["Not found", "__plaza_internal_not_found"];
                }
            });

            if (options.length === 0){
                throw Error("No options found for dynamic argument: " + arg);
            }
        }
        catch(exception) {
            console.error(exception);
            options = [["Not found", "__plaza_internal_not_found"]];
        }

        const resolved = dynamicArg as ResolvedDynamicBlockArgument;
        resolved.options = options;

        return resolved;
    }

    getCallbackCacheId(arg: DynamicBlockArgument, block: CustomBlock): string {
        return (CustomBlockService.DataCallbackCachePrefix
                + '/'
                + block.service_port_id
                + '/'
                + arg.callback);
    }

    async getCachedArgOptions(arg: DynamicBlockArgument, block: CustomBlock): Promise<[string, string][]> {
        const storage = window.localStorage;
        const results = storage.getItem(this.getCallbackCacheId(arg, block));

        if ((results === null) || (results.length == 0)) {
            return [["Loading", "__plaza_internal_loading"]];
        }
        return JSON.parse(results);
    }

    async cacheArgOptions(arg: DynamicBlockArgument, block: CustomBlock, result: [string, string][]) {
        const storage = window.localStorage;
        const results = storage.setItem(this.getCallbackCacheId(arg, block),
                                        JSON.stringify(result));
    }

    async getArgOptions(arg: DynamicBlockArgument, block: CustomBlock): Promise<[string, string][]> {
        const userApiRoot = await this.sessionService.getApiRootForUserId();

        return (this.http.get(userApiRoot + '/bridges/id/' + block.service_port_id + '/callback/' + arg.callback,
            {
                headers: this.sessionService.getAuthHeader(),
            })
            .map((response: { result: { [key: string]: { name: string } } }) => {
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
