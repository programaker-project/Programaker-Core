import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Session } from './session';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import * as progbar from './ui/progbar';
import * as API from './api-config';
import { Observable } from 'rxjs/Observable';
import { ApiRoot } from './api-config';
import { ContentType } from './content-type';

@Injectable()
export class SessionService {
    static EstablishedSession: Session = undefined;
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

    removeToken() {
        const storage = window.localStorage;
        storage.removeItem('session.service.token');
    }

    getToken(): string {
        const storage = window.localStorage;
        const token = storage.getItem('session.service.token');

        return token;
    }

    async getUserApiRoot(): Promise<string> {
        // tslint:disable-next-line:no-debugger
        let session = SessionService.EstablishedSession;
        if (session === undefined) {
            session = await this.getSession();
        }

        return this.getApiRootForUser(session.username);
    }

    getApiRootForUser(username: string): string {
        return API.ApiRoot + '/users/' + username;
    }

    getApiRootForUserId(user_id: string): string {
        return API.ApiRoot + '/users/id/' + user_id;
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
        return this.addContentType(headers, ContentType.Json);
    }

    addContentType(headers: HttpHeaders, contentType: ContentType): HttpHeaders {
        return headers.append('content-type', contentType);
    }

    getSession(): Promise<Session> {
      if (this.getToken() === null) {
        return Promise.resolve(null);
      }

      return (this.http
              .get(this.checkSessionUrl, {headers: this.getAuthHeader()})
              .map((response) => {
                  const check = response as any;
                  const session = new Session(check.success,
                                              check.username,
                                              check.user_id);
                  SessionService.EstablishedSession = session;

                  return session;
              })
              .toPromise());
    }

    login(username: string, password: string): Promise<boolean> {
        return progbar.track(this.http
                .post(this.loginUrl,
                      JSON.stringify({username: username, password: password}),
                      {headers: this.addJsonContentType(this.getAuthHeader())})
                .map(response => {
                  const data = response as any;
                  if (data.success) {
                      this.storeToken(data.token);

                      const newSession = new Session(true, username);

                      return true;
                  }
                  return false;
                })
                .toPromise());
    }

    logout() {
        this.removeToken();
        SessionService.EstablishedSession = undefined;
    }


    register(username: string, email: string, password: string): Promise<boolean> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                .post(
                  this.registerUrl,
                  JSON.stringify({ username: username
                                 , password: password
                                 , email: email
                                 }),
                  { headers  })
                .map(response => {
                    return true;
                })
                .toPromise());
    }


}
