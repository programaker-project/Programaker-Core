import { Injectable } from '@angular/core';
import { Http } from '@angular/http';
import { Session } from './session';
import 'rxjs/add/operator/toPromise';
import { ApiRoot } from './api-config';

@Injectable()
export class SessionService {
    checkSessionUrl = "/api/check_session";
    loginUrl = "/api/login";
    registerUrl = "/api/register";

    constructor(
        private http: Http,
    ) {
        this.http = http;
    }

    getSession(): Promise<Session> {
        return (this.http
                .get(this.checkSessionUrl)
                .map(response => response.json())
                .toPromise());
    }


    login(username: string, password: string): Promise<boolean> {
        return (this.http
                .post(this.loginUrl, {username: username, password: password})
                .map(response => response.json().success)
                .toPromise());
    }


    register(username: string, password: string): Promise<boolean> {
        return (this.http
                .post(this.registerUrl, {username: username, password: password})
                .map(response => response.json().success)
                .toPromise());
    }


}
