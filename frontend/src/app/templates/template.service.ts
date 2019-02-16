import { Injectable } from '@angular/core';
import { Template } from './template';
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
}
