import {map} from 'rxjs/operators';
import { Injectable } from '@angular/core';

import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { CustomBlock, ResolvedCustomBlock, ResolvedBlockArgument, BlockArgument, DynamicBlockArgument, StaticBlockArgument, ResolvedDynamicBlockArgument, DynamicSequenceBlockArgument, ResolvedDynamicSequenceBlockArgument } from './custom_block';
import { Observable } from 'rxjs';
import { BrowserService } from './browser.service';
import { EnvironmentService } from './environment.service';


type CallbackResult = (
    { [key: string]: { name: string } }   // Old style
    | [{ id: string, name: string }]  // New style
);



@Injectable()
export class CustomBlockService {
    static DataCallbackCachePrefix = "programaker-data-callback";
    onFlightCallbackQueries: { [key: string]: Promise<[string, string][]> } = {};

    constructor(
        private http: HttpClient,
        private browser: BrowserService,

        private sessionService: SessionService,
        private environmentService: EnvironmentService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
        this.onFlightCallbackQueries = {};
    }

    private async getCustomBlocksUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/custom-blocks/';
    }

    private getCustomBlocksOnProgramUrl(programId: string) {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/custom-blocks`;
    }

    public async getCustomBlocksOnProgram(programId: string, skip_resolve_argument_options?: boolean): Promise<ResolvedCustomBlock[]> {
        const url = this.getCustomBlocksOnProgramUrl(programId);

        const response = this.http.get(url,
                                       {
            headers: this.sessionService.addJsonContentType(
                this.sessionService.getAuthHeader())
        });

        return this._resolveCustomBlocksAnswer(programId, response, !!skip_resolve_argument_options);
    }

    private async _resolveCustomBlocksAnswer(programId: string,
                                             customBlocksResponse: Observable<Object>,
                                             skip_resolve_argument_options: boolean): Promise<ResolvedCustomBlock[]> {
        const blocks = await customBlocksResponse.pipe(
            map((response: { [key: string]: any[] }) => {
                const blocks: CustomBlock[] = [];
                for (const service_port_id of Object.keys(response)) {
                    for (const block of response[service_port_id]) {
                        block.service_port_id = service_port_id;
                        block.id = 'services.' + service_port_id + '.' + block.block_id;

                        blocks.push(block);
                    }
                }

                return blocks;
            })).toPromise();

        const resolvedBlocks = await Promise.all(blocks.map(async (block): Promise<ResolvedCustomBlock> => {
            try {
                return await this._resolveBlock(programId, block, skip_resolve_argument_options === true);
            }
            catch(err) {
                console.warn(err);
            }
        }));

        return resolvedBlocks.filter((v, _i, _a) => {
            return v !== undefined;
        });
    }

    private _fixBlockErrors(block: ResolvedCustomBlock): ResolvedCustomBlock {
        try {
            const regexp = RegExp(/%\d/g);

            let arguments_in_message = 0;
            let match: RegExpExecArray;
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

    private async _resolveBlock(programId: string, block: CustomBlock, skip_resolve_argument_options: boolean): Promise<ResolvedCustomBlock> {
        // Note that the block is not copied, the resolution is done destructively!
        const resolvedBlock = block as ResolvedCustomBlock;

        if (!skip_resolve_argument_options) {
            const newArguments: ResolvedBlockArgument[] = [];
            for (const argument of block.arguments) {
                newArguments.push(await this._resolveArgument(programId, argument, block));
            }

            resolvedBlock.arguments = newArguments;
        }

        this._fixBlockErrors(resolvedBlock);

        return resolvedBlock;
    }

    private async _resolveArgument(programId: string, arg: BlockArgument, block: CustomBlock): Promise<ResolvedBlockArgument> {
        // Note that the argument is not copied, the resolution is done destructively!
        if (!((arg as DynamicBlockArgument).callback || ((arg as DynamicSequenceBlockArgument).callback_sequence)) ) {
            return arg as StaticBlockArgument;
        }

        if ((arg as DynamicSequenceBlockArgument).callback_sequence) {
            (arg as any).callback = (arg as any).callback_sequence[0];
        }

        const dynamicArg = arg as DynamicBlockArgument;
        let options : [string, string][];
        try {
            options = await this.getCachedArgOptions(programId, dynamicArg, block);
            const loading = options[0][1] === '__programaker_internal_loading';

            // Reload asynchronously
            this.getArgOptions(programId, block.service_port_id, dynamicArg.callback).then((result) => {
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
                    options[0] = ["Not found", "__programaker_internal_not_found"];
                }
            });

            if (options.length === 0){
                throw Error("No options found for dynamic argument: " + arg);
            }
        }
        catch(exception) {
            console.error("Callback error:", exception);
            options = [["Not found", "__programaker_internal_not_found"]];
        }

        const resolved = dynamicArg as ResolvedDynamicBlockArgument;

        if ((arg as DynamicSequenceBlockArgument).callback_sequence) {
            (resolved as any as ResolvedDynamicSequenceBlockArgument).first_level_options = options;
            (resolved as any as ResolvedDynamicSequenceBlockArgument).bridge_id = block.service_port_id;
            (resolved as any as ResolvedDynamicSequenceBlockArgument).program_id = programId;
        }
        else {
            resolved.options = options;
        }


        return resolved;
    }

    private getCallbackCacheId(programId: string, bridgeId: string, callbackName: string): string {
        return (CustomBlockService.DataCallbackCachePrefix
                + programId
                + '/'
                + bridgeId
                + '/'
                + callbackName);
    }

    private async getCachedArgOptions(programId: string, arg: DynamicBlockArgument, block: CustomBlock): Promise<[string, string][]> {
        const storage = this.browser.window.sessionStorage;
        const results = storage.getItem(this.getCallbackCacheId(programId, block.service_port_id, arg.callback));

        if ((results === null) || (results.length == 0)) {
            return [["Loading", "__programaker_internal_loading"]];
        }
        return JSON.parse(results);
    }

    private async cacheArgOptions(programId: string, bridgeId: string, callbackName: string, result: [string, string][]) {
        const storage = this.browser.window.sessionStorage;
        const results = storage.setItem(this.getCallbackCacheId(programId, bridgeId, callbackName),
                                        JSON.stringify(result));
    }

    public async getCallbackOptions(programId: string, bridgeId: string, callbackName: string): Promise<[string, string][]> {
        try {
            const result = await this.getArgOptions(programId, bridgeId, callbackName);
            // Awaited here to catch the error
            return result;
        }
        catch (err) {
            // Pull from disk cache if there's an error on the service
            const storage = this.browser.window.sessionStorage;
            const results = storage.getItem(this.getCallbackCacheId(programId, bridgeId, callbackName));

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

    private reformatCallbackResult(result: CallbackResult): [string, string][] {
        const options: [string, string][] = [];
        if (result.constructor == Object) {
            // Data from callback as dictionary
            const resultDict = result as { [key: string]: { name: string } };

            for (const key of Object.keys(resultDict)) {
                options.push([resultDict[key].name || key, key]);
            }
        }
        else {
            // Data from callback as list
            const resultList = result as [ {id: string, name: string} ];

            for (const item of resultList) {
                options.push([item.name, item.id]);
            }
        }
        return options;
    }

    // Note that this values are NOT cached
    public getCallbackOptionsOnSequence(programId: string, bridgeId: string, callbackName: string, sequenceId: string): Promise<[string, string][]> {
        const url = `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/bridges/by-id/${bridgeId}/callbacks/${callbackName}`;

        return (this.http.get(url, {
            headers: this.sessionService.getAuthHeader(),
            params: { sequence_id: sequenceId }
        }).toPromise()
            .then((response: { result: CallbackResult } ) => {
                return this.reformatCallbackResult(response.result);
            }));
    }

    private async getArgOptions(programId: string, bridgeId: string, callbackName: string): Promise<[string, string][]> {
        const url = `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/bridges/by-id/${bridgeId}/callbacks/${callbackName}`;

        // Already being performed, "attach" to the on flight query
        if (this.onFlightCallbackQueries[url] !== undefined) {
            return this.onFlightCallbackQueries[url];
        }

        const query = (this.http.get(url, { headers: this.sessionService.getAuthHeader() }).toPromise()
            .then((response: { result: CallbackResult } ) => {
                return this.reformatCallbackResult(response.result);
            }));

        // Cache result

        query
            .then((result: [string, string][]) => {
                this.cacheArgOptions(programId, bridgeId, callbackName, result);
            })
            .catch(err => {
                console.warn(err);
                delete this.onFlightCallbackQueries[url];
            });

        this.onFlightCallbackQueries[url] = query;
        return query;
    }
}
