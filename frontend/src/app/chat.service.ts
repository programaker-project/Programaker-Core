import { Injectable } from '@angular/core';
import { Chat } from './chat';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { SessionService } from './session.service';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class ChatService {

    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getListChatsUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/chats/';
    }

    getAvailableChats(): Promise<Chat[]> {
        return this.getListChatsUrl().then(
            url => this.http.get(url, { headers: this.sessionService.getAuthHeader()})
                            .map(response => response as Chat[])
                            .toPromise());
    }
}
