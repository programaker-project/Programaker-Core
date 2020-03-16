import { Observable, Observer } from 'rxjs';
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Session } from './session';

import * as progbar from './ui/progbar';
import * as API from './api-config';
import { ContentType } from './content-type';

export type SessionInfoUpdate = { loggedIn: boolean };

@Injectable()
export class SessionService {

    readonly checkSessionUrl = API.ApiRoot + '/sessions/check';
    readonly loginUrl = API.ApiRoot + '/sessions/login';
    readonly registerUrl = API.ApiRoot + '/sessions/register';
    readonly registerValidateUrl = API.ApiRoot + '/sessions/register/verify';
    readonly resetPasswordUrl = API.ApiRoot + '/sessions/login/reset';
    readonly validatePasswordUpdateUrl = API.ApiRoot + '/sessions/login/reset/validate';
    readonly passwordUpdateUrl = API.ApiRoot + '/sessions/login/reset/update';


    // These static values help create a Singleton-like class.
    // While not strtictly following the Singleton pattern, as class instances
    //   are obtained through Dependency Injection, it allows for every instance
    //   of the class to share the relevant state.
    // The shared state is the established session, it's Observable and Observer.
    static EstablishedSession: Session = null;
    static sessionInfoObservable: Observable<SessionInfoUpdate> = null;
    private static _sessionInfoObserver: Observer<SessionInfoUpdate> = null;

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
        let session = SessionService.EstablishedSession;
        if (session === null) {
            session = await this.getSession();
        }

        return this.getApiRootForUser(session.username);
    }

    async updateUserSettings(user_settings_update : { is_advanced?: boolean }): Promise<boolean> {
        const url = (await this.getApiRootForUserId()) + '/settings';
        const response = await (this.http
                                .post(url, JSON.stringify(user_settings_update),
                                      { headers: this.addJsonContentType(this.getAuthHeader()) })
                                .toPromise());

        return (response as { success: boolean }).success;
    }

    getApiRootForUser(username: string): string {
        return API.ApiRoot + '/users/' + username;
    }

    async getApiRootForUserId(user_id?: string): Promise<string> {
        if (!user_id) {
            let session = SessionService.EstablishedSession;
            if (session === undefined) {
                session = await this.getSession();
            }
            user_id = session.user_id;
        }
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

    async getSessionMonitor(): Promise<{session: Session, monitor: Observable<SessionInfoUpdate>}> {
        if (!SessionService.sessionInfoObservable) {
            this._monitorSession();
        }

        return {
            session: await this.getSession(),
            monitor: SessionService.sessionInfoObservable,
        };
    }

    getSession(): Promise<Session> {
        if (this.getToken() === null) {
            return Promise.resolve(null);
        }

        if (SessionService.EstablishedSession !== null) {
            return Promise.resolve(SessionService.EstablishedSession);
        }

        return this.forceUpdateSession();
    }

    login(username: string, password: string): Promise<boolean> {
        return progbar.track(this.http
                             .post(this.loginUrl,
                                   JSON.stringify({ username: username, password: password }),
                                   { headers: this.addJsonContentType(this.getAuthHeader()) }).toPromise()
                             .then(async response => {
                                 const data = response as any;
                                 if (data.success) {

                                     this.storeToken(data.token);
                                     await this.forceUpdateSession();

                                     return true;
                                 }
                                 return false;
                             }));
    }

    logout() {
        this.removeToken();
        this._updateSession(null);
    }

    register(username: string, email: string, password: string): Promise<{ success: boolean, continue_to_login: boolean}> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(
            this.http
                .post(
                    this.registerUrl,
                    JSON.stringify({
                        username: username
                        , password: password
                        , email: email
                    }),
                    { headers }).toPromise()
                .then(response => {
                    return {
                        success: (response as any).success,
                        continue_to_login: (response as any).ready
                    }}));
    }

    validateRegisterCode(verificationCode: string): Promise<Session> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                             .post(
                                 this.registerValidateUrl,
                                 JSON.stringify({
                                     verification_code: verificationCode
                                 }),
                                 { headers }).toPromise()
                             .then(response => {
                                 const success = (response as any).success;
                                 if (!success) {
                                     throw new Error(success.message);
                                 }

                                 this.storeToken((response as any).session.token);
                                 return this.forceUpdateSession();
                             }));
    }

    requestResetPassword(email: string): Promise<void> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                             .post(
                                 this.resetPasswordUrl,
                                 JSON.stringify({
                                     email: email
                                 }),
                                 { headers }).toPromise()
                             .then(_response => {
                                 return;
                             }));
    }

    validatePasswordUpdateCode(verificationCode: string): Promise<void> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                             .post(
                                 this.validatePasswordUpdateUrl,
                                 JSON.stringify({
                                     verification_code: verificationCode
                                 }),
                                 { headers }).toPromise()
                             .then(_response => {
                                 return;
                             }));
    }

    resetPasswordUpdate(verificationCode: string, password: string): Promise<void> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                             .post(
                                 this.passwordUpdateUrl,
                                 JSON.stringify({
                                     verification_code: verificationCode,
                                     password: password,
                                 }),
                                 { headers }).toPromise()
                             .then(_response => {
                                 return;
                             }));
    }

    private _monitorSession() {
        const service = this;

        SessionService.sessionInfoObservable = new Observable((observer) => {
            SessionService._sessionInfoObserver = observer;
            // This will be operated from `_updateSession`
        });
    }

    private _updateSession(session: Session) {
        SessionService.EstablishedSession = session;
        const loggedIn = session !== null;

        if (SessionService._sessionInfoObserver) {
            SessionService._sessionInfoObserver.next({ loggedIn: loggedIn });
        }
    }

    public async forceUpdateSession(): Promise<Session> {
        if (this.getToken() === null) {
            return Promise.resolve(null);
        }

        const response = (await this.http
                          .get(this.checkSessionUrl, { headers: this.getAuthHeader() }).toPromise());

        const check = response as any;
        const session = new Session(check.success,
                                    check.username,
                                    check.user_id,
                                    check.tags);
        this._updateSession(session);

        return session;
    }
}
