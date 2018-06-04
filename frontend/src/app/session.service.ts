import { Injectable } from '@angular/core';
import { Http, Headers } from '@angular/http';
import { Session } from './session';
import 'rxjs/add/operator/toPromise';
import * as API from './api-config';

@Injectable()
export class SessionService {
    checkSessionUrl = API.ApiRoot + '/sessions/check';
    loginUrl = API.ApiRoot + '/sessions/login';
    registerUrl = API.ApiRoot + '/sessions/register';

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


    register(username: string, email: string, password: string): Promise<boolean> {
        const headers = new Headers();
        headers.append('Content-Type', 'application/json');

        return (this.http
                .post(
                  this.registerUrl,
                  JSON.stringify({ username: username
                                 , password: password
                                 , email: email
                                 }),
                  { headers  })
                .map(response => response.json().success)
                .toPromise());
    }


}
