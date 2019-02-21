import { Injectable } from '@angular/core';
import { Template, TemplateCreationResponse } from './template';
import * as API from '../api-config';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { HttpClient } from '@angular/common/http';
import { SessionService } from '../session.service';
import { ContentType } from '../content-type';

@Injectable()
export class TemplateService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getTemplateIndexUrl(): Promise<string> {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/templates/';
    }

    async getSpecificTemplateUrl(user_id: string, template_id: string): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId(user_id);

        return root + '/templates/id/' + template_id;
    }

    async saveTemplate(template_name: string, template_content: any[]): Promise<TemplateCreationResponse> {
        const indexUrl = await this.getTemplateIndexUrl();

        return this.http
            .post(indexUrl, JSON.stringify({}),
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                })
            .map(response => {
                return response as TemplateCreationResponse;
            })
            .toPromise();

    }

}
