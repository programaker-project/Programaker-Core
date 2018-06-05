import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Session } from './session';
import 'rxjs/add/operator/toPromise';
import * as API from './api-config';

@Injectable()
export class SessionService {
    checkSessionUrl = API.ApiRoot + '/sessions/check';
    loginUrl = API.ApiRoot + '/sessions/login';
    registerUrl = API.ApiRoot + '/sessions/register';

    constructor(
        private http: HttpClient,
    ) {
        this.http = http;
    }

    storeToken(token: string) {
        const storage = window.localStorage;
        storage.setItem('session.service.token', token);
    }

    getToken(): string {
        const storage = window.localStorage;
        const token = storage.getItem('session.service.token');

        return token;
    }

    getAuthHeader(): HttpHeaders {
      const headers = new HttpHeaders();

      const token = this.getToken();
      if (token !== null) {
         return headers.append('authorization', token);
      }

      return headers;
    }

    addJsonContentType(headers: HttpHeaders): HttpHeaders {
      return headers.append('content-type', 'application/json');
    }

    getSession(): Promise<Session> {
      if (this.getToken() === null){
        return Promise.resolve(null);
      }

      return (this.http
              .get(this.checkSessionUrl, {headers: this.getAuthHeader()})
              .map((response) => {
                  const session = response as Session;
                  return session;
              })
              .toPromise());
    }

    login(username: string, password: string): Promise<boolean> {
        return (this.http
                .post(this.loginUrl,
                      JSON.stringify({username: username, password: password}),
                      {headers: this.addJsonContentType(this.getAuthHeader())})
                .map(response => {
                  const data = response as any;
                  if (data.success) {
                      this.storeToken(data.token);
                      return true;
                  }
                  return false;
                })
                .toPromise());
    }


    register(username: string, email: string, password: string): Promise<boolean> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return (this.http
                .post(
                  this.registerUrl,
                  JSON.stringify({ username: username
                                 , password: password
                                 , email: email
                                 }),
                  { headers  })
                .map(response => (response as any).success)
                .toPromise());
    }


}
