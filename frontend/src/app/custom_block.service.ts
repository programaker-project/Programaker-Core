import { Injectable } from '@angular/core';
import { ServicePortMetadata } from './service_port';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';
import { CustomBlock } from './custom_block';

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

    getCustomBlocks(): Promise<[CustomBlock]> {
        return this.getCustomBlocksUrl().then(url =>
            this.http.get(url,
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                })
                .map(response => {
                    console.log("Blocks:", response);
                    return response as [CustomBlock];
                })
                .toPromise());
    }

}
