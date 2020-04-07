
import {map} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import * as API from './api-config';


import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';
import { CustomBlock, ResolvedCustomBlock, ResolvedBlockArgument, BlockArgument, DynamicBlockArgument, StaticBlockArgument, ResolvedDynamicBlockArgument } from './custom_block';


type CallbackResult = (
    { [key: string]: { name: string } }   // Old style
        | [ {id: string, name: string} ]  // New style
);



@Injectable()
export class CustomBlockService {
    static DataCallbackCachePrefix = "plaza-data-callback";
    onFlightCallbackQueries: {[key: string]: Promise<[string, string][]>} = {};

    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
        this.onFlightCallbackQueries = {};
    }

    private async getCustomBlocksUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/custom-blocks/';
    }

    public async getCustomBlocks(skip_resolve_argument_options?: boolean): Promise<ResolvedCustomBlock[]> {
        const blocks = await this.getCustomBlocksUrl().then(url =>
            this.http.get(url,
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                }).pipe(
                map(response => {
                    const blocks: CustomBlock[] = [];
                    for (const service_port_id of Object.keys(response)) {
                        for (const block of response[service_port_id]) {
                            block.service_port_id = service_port_id;
                            block.id = 'services.' + service_port_id + '.' + block.block_id;

                            blocks.push(block);
                        }
                    }

                    console.debug("Blocks:", blocks);
                    return blocks;
                }))
                .toPromise());

        const resolvedBlocks = await Promise.all(blocks.map(async (block): Promise<ResolvedCustomBlock> => {
            return await this._resolveBlock(block, skip_resolve_argument_options === true);
        }));

        return resolvedBlocks.filter((v, _i, _a) => {
            return v !== undefined;
        });
    }

    private _fixBlockErrors(block: ResolvedCustomBlock): ResolvedCustomBlock {
        try {
            const regexp = RegExp(/%\d/g);

            let arguments_in_message = 0;
            let match;
            while ((match = regexp.exec(block.message as any)) !== null) {
                arguments_in_message++;
            }

            let arguments_declared = block.arguments.length;

            // Validate & fill arguments declared
            if (arguments_in_message > arguments_declared) {
                console.error("(arguments_in_message > arguments_declared) on ", block);
            }

            while (arguments_in_message > arguments_declared) {
                block.arguments.push({
                    type: "string",
                    class: undefined,
                    default_value: "change_this",
                });
                arguments_declared++;
            }

            // Validate & fill arguments in message
            if (arguments_in_message < arguments_declared) {
                console.warn(`(${arguments_in_message} < ${arguments_declared}) arguments_in_message < arguments_declared on `, block);
            }

            while (arguments_in_message < arguments_declared) {
                block.message += ` %${arguments_in_message + 1} `;
                arguments_in_message++;
            }
        }
        catch(err) {
            console.error(err);
            return undefined;
        }

        return block;
    }

    private async _resolveBlock(block: CustomBlock, skip_resolve_argument_options: boolean): Promise<ResolvedCustomBlock> {
        // Note that the block is not copied, the resolution is done destructively!
        const resolvedBlock = block as ResolvedCustomBlock;

        if (!skip_resolve_argument_options) {
            const newArguments: ResolvedBlockArgument[] = [];
            for (const argument of block.arguments) {
                newArguments.push(await this._resolveArgument(argument, block));
            }

            resolvedBlock.arguments = newArguments;
        }

        this._fixBlockErrors(resolvedBlock);

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
            this.getArgOptions(block.service_port_id, dynamicArg.callback).then((result) => {
                if (result.length === 0){
                    throw Error("No options found for dynamic argument: " + arg);
                }

                // Replace all options in place
                let index: number;
                for (index = 0; index < result.length; index++) {
                    options[index] = result[index];
                }

                while (options.length > index) {
                    options.pop();
                }
            }).catch((err) => {
                console.warn(err);
                if (loading) {
                    options[0] = ["Not found", "__plaza_internal_not_found"];
                }
            });

            if (options.length === 0){
                throw Error("No options found for dynamic argument: " + arg);
            }
        }
        catch(exception) {
            console.error("Callback error:", exception);
            options = [["Not found", "__plaza_internal_not_found"]];
        }

        const resolved = dynamicArg as ResolvedDynamicBlockArgument;
        resolved.options = options;

        return resolved;
    }

    private getCallbackCacheId(bridge_id: string, callback_name: string): string {
        return (CustomBlockService.DataCallbackCachePrefix
                + '/'
                + bridge_id
                + '/'
                + callback_name);
    }

    private async getCachedArgOptions(arg: DynamicBlockArgument, block: CustomBlock): Promise<[string, string][]> {
        const storage = window.localStorage;
        const results = storage.getItem(this.getCallbackCacheId(block.service_port_id, arg.callback));

        if ((results === null) || (results.length == 0)) {
            return [["Loading", "__plaza_internal_loading"]];
        }
        return JSON.parse(results);
    }

    private async cacheArgOptions(bridge_id: string, callback_name: string, result: [string, string][]) {
        const storage = window.localStorage;
        const results = storage.setItem(this.getCallbackCacheId(bridge_id, callback_name),
                                        JSON.stringify(result));
    }

    public async getCallbackOptions(bridge_id: string, callback_name: string): Promise<[string, string][]> {
        try {
            const result = await this.getArgOptions(bridge_id, callback_name);
            // Awaited here to catch the error
            return result;
        }
        catch (err) {
            // Pull from disk cache if there's an error on the service
            const storage = window.localStorage;
            const results = storage.getItem(this.getCallbackCacheId(bridge_id, callback_name));

            // Check if it's in cache
            if ((results !== null) && (results.length > 0)) {
                // If it is, log the error and send results back
                console.log("Error getting callback options:", err);

                return JSON.parse(results);
            }

            // Else, we cannot handle it, pass it over
            throw err;
        }
    }

    private async getArgOptions(bridge_id: string, callback_name: string): Promise<[string, string][]> {
        const userApiRoot = await this.sessionService.getApiRootForUserId();
        const url = userApiRoot + '/bridges/id/' + bridge_id + '/callback/' + callback_name;

        // Already being performed, "attach" to the on flight query
        if (this.onFlightCallbackQueries[url] !== undefined) {
            return this.onFlightCallbackQueries[url];
        }

        const query = (this.http.get(url, { headers: this.sessionService.getAuthHeader() }).toPromise()
            .then((response: { result: CallbackResult } ) => {
                if (response.result.constructor == Object) {
                    // Data from callback as dictionary
                    const options = [];
                    const result = response.result as { [key: string]: { name: string } };

                    for (const key of Object.keys(result)) {
                        options.push([result[key].name || key, key]);
                    }

                    return options;
                }
                else {
                    // Data from callback as list
                    // Data from callback as dictionary
                    const options = [];
                    const result = response.result as [ {id: string, name: string} ];

                    for (const item of result) {
                        options.push([item.name, item.id]);
                    }

                    return options;
                }
            }));

        // Cache result

        query.then(result => {
            this.cacheArgOptions(bridge_id, callback_name, result);
        });



        query.catch(err => {
            console.error(err);
            delete this.onFlightCallbackQueries[url];
        });

        this.onFlightCallbackQueries[url] = query;
        return query;
    }
}
