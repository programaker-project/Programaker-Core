import { Injectable } from '@angular/core';
import { Bot } from './bot';
import { Http } from '@angular/http';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';

@Injectable()
export class BotService {
    private getBotsUrl = API.ApiRoot + '/bots/';

    constructor(
        private http: Http,
    ) {
        this.http = http;
    }

    getBots(): Promise<Bot[]> {
        return (this.http
                .get(this.getBotsUrl)
                .map(response => response.json())
                .toPromise());
    }

    getBot(id: number): Promise<Bot> {
        return this.getBots()
            .then((bots) => bots.find((bot) => bot.id === id));
    }
}
