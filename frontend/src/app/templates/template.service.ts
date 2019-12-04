import { Injectable } from '@angular/core';
import { Template, TemplateCreationResponse } from './template';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { HttpClient } from '@angular/common/http';
import { SessionService } from '../session.service';

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
        const root = await this.sessionService.getApiRootForUserId();
        return root + '/templates/';
    }

    async getSpecificTemplateUrl(user_id: string, template_id: string): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId(user_id);

        return root + '/templates/id/' + template_id;
    }

    async saveTemplate(template_name: string, template_content: any[]): Promise<TemplateCreationResponse> {
        const indexUrl = await this.getTemplateIndexUrl();

        return this.http
            .post(indexUrl, JSON.stringify({
                name: template_name,
                content: template_content,
            }),
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                })
            .map(response => {
                return response as TemplateCreationResponse;
            })
            .toPromise();
    }

    async getTemplates(): Promise<Template[]> {
        const indexUrl = await this.getTemplateIndexUrl();

        return this.http
            .get(indexUrl, {
                headers: this.sessionService.getAuthHeader()
            })
            .map(response => {
                return response as Template[];
            })
            .toPromise();
    }

}
