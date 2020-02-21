
import {map} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Session } from './session';


import * as progbar from './ui/progbar';
import * as API from './api-config';
import { ContentType } from './content-type';

@Injectable()
export class SessionService {
    static EstablishedSession: Session = undefined;
    checkSessionUrl = API.ApiRoot + '/sessions/check';
    loginUrl = API.ApiRoot + '/sessions/login';
    registerUrl = API.ApiRoot + '/sessions/register';
    registerValidateUrl = API.ApiRoot + '/sessions/register/verify';
    resetPasswordUrl = API.ApiRoot + '/sessions/login/reset';
    validatePasswordUpdateUrl = API.ApiRoot + '/sessions/login/reset/validate';
    passwordUpdateUrl = API.ApiRoot + '/sessions/login/reset/update';

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
        if (session === undefined) {
            session = await this.getSession();
        }

        return this.getApiRootForUser(session.username);
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

    getSession(): Promise<Session> {
        if (this.getToken() === null) {
            return Promise.resolve(null);
        }

        return (this.http
            .get(this.checkSessionUrl, { headers: this.getAuthHeader() }).pipe(
            map((response) => {
                const check = response as any;
                const session = new Session(check.success,
                    check.username,
                    check.user_id);
                SessionService.EstablishedSession = session;

                return session;
            }))
            .toPromise());
    }

    login(username: string, password: string): Promise<boolean> {
        return progbar.track(this.http
            .post(this.loginUrl,
                JSON.stringify({ username: username, password: password }),
                { headers: this.addJsonContentType(this.getAuthHeader()) }).pipe(
            map(response => {
                const data = response as any;
                if (data.success) {
                    this.storeToken(data.token);

                    const newSession = new Session(true, username, data.user_id);
                    SessionService.EstablishedSession = newSession;

                    return true;
                }
                return false;
            }))
            .toPromise());
    }

    logout() {
        this.removeToken();
        SessionService.EstablishedSession = undefined;
    }


    register(username: string, email: string, password: string): Promise<{ success: boolean, continue_to_login: boolean}> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                             .post(
                                 this.registerUrl,
                                 JSON.stringify({
                                     username: username
                                     , password: password
                                     , email: email
                                 }),
                                 { headers }).pipe(
                             map(response => {
                                 return {
                                     success: (response as any).success,
                                     continue_to_login: (response as any).ready
                                 };
                             }))
                             .toPromise());
    }


    validateRegisterCode(verificationCode: string): Promise<Session> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                             .post(
                                 this.registerValidateUrl,
                                 JSON.stringify({
                                     verification_code: verificationCode
                                 }),
                                 { headers }).pipe(
                             map(response => {
                                 const success = (response as any).success;
                                 if (!success) {
                                     throw new Error(success.message);
                                 }


                                 this.storeToken((response as any).session.token);
                                 const session = new Session(true,
                                                             (response as any).session.username,
                                                             (response as any).session.user_id);
                                 SessionService.EstablishedSession = session;
                                 return session;
                             }))
                             .toPromise());
    }

    requestResetPassword(email: string): Promise<void> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                             .post(
                                 this.resetPasswordUrl,
                                 JSON.stringify({
                                     email: email
                                 }),
                                 { headers }).pipe(
                             map(_response => {
                                 return;
                             }))
                             .toPromise());
    }

    validatePasswordUpdateCode(verificationCode: string): Promise<void> {
        const headers = this.addJsonContentType(new HttpHeaders());

        return progbar.track(this.http
                             .post(
                                 this.validatePasswordUpdateUrl,
                                 JSON.stringify({
                                     verification_code: verificationCode
                                 }),
                                 { headers }).pipe(
                             map(_response => {
                                 return;
                             }))
                             .toPromise());
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
                                 { headers }).pipe(
                             map(_response => {
                                 return;
                             }))
                             .toPromise());
    }
}
