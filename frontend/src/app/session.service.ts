import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Session } from './session';
import 'rxjs/add/operator/toPromise';
import * as API from './api-config';
import { Observable } from 'rxjs/Observable';
import { Subscriber, ReplaySubject } from 'rxjs';

@Injectable()
export class SessionService {
    checkSessionUrl = API.ApiRoot + '/sessions/check';
    loginUrl = API.ApiRoot + '/sessions/login';
    registerUrl = API.ApiRoot + '/sessions/register';
    sessionObservable: ReplaySubject<Session>;

    constructor(
        private http: HttpClient,
    ) {
        this.http = http;

        this.sessionObservable = new ReplaySubject<Session>(1);

        // Prefill the replayer
        this.sessionObservable.next(new Session(false, null));
    }

    observe(): ReplaySubject<Session> {
        return this.sessionObservable;
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
      if (this.getToken() === null) {
        return Promise.resolve(null);
      }

      return (this.http
              .get(this.checkSessionUrl, {headers: this.getAuthHeader()})
              .map((response) => {
                  const check = response as any;
                  const session = new Session(check.success, check.username);

                  this.sessionObservable.next(session);

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

                      const newSession = new Session(true, username);
                      this.sessionObservable.next(newSession);

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
                .map(response => {
                    if ((response as any).success) {
                        this.sessionObservable.next(new Session(true, username));
                        return true;
                    }

                    return false;
                })
                .toPromise());
    }


}
