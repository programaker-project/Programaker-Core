import { Observable, Observer } from 'rxjs';
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Session } from './session';

import * as progbar from './ui/progbar';
import { ContentType } from './content-type';
import { BrowserService } from './browser.service';
import { CookiesService } from '@ngx-utils/cookies';
import { EnvironmentService } from './environment.service';

export type SessionInfoUpdate = { session: Session };

@Injectable()
export class SessionService {

    private readonly COOKIE_TOKEN_KEY = 'programaker-auth';


    // These static values help create a Singleton-like class.
    // While not strtictly following the Singleton pattern, as class instances
    //   are obtained through Dependency Injection, it allows for every instance
    //   of the class to share the relevant state.
    // The shared state is the established session, it's Observable and Observer.
    static EstablishedSession: Session = null;
    static sessionInfoObservable: Observable<SessionInfoUpdate> = null;
    private static _sessionInfoObserver: Observer<SessionInfoUpdate> = null;
    static EstablishmentPromise: Promise<Session> = null;

    constructor(
        private http: HttpClient,
        private browser: BrowserService,
        private cookies: CookiesService,
        private environmentService: EnvironmentService,
    ) {
        this.http = http;
    }

    getCheckSessionUrl(): string {
        return this.environmentService.getApiRoot() + '/sessions/check';
    }
    getLoginUrl(): string {
        return this.environmentService.getApiRoot() + '/sessions/login';
    }
    getRegisterUrl(): string {
        return this.environmentService.getApiRoot() + '/sessions/register';
    }
    getRegisterValidateUrl(): string {
        return this.environmentService.getApiRoot() + '/sessions/register/verify';
    }
    getResetPasswordUrl(): string {
        return this.environmentService.getApiRoot() + '/sessions/login/reset';
    }
    getValidatePasswordUpdateUrl(): string {
        return this.environmentService.getApiRoot() + '/sessions/login/reset/validate';
    }
    getPasswordUpdateUrl(): string {
        return this.environmentService.getApiRoot() + '/sessions/login/reset/update';
    }
    getUploadAssetToProgramIdUrl(programId: string): string {
        return this.environmentService.getApiRoot() + `/programs/by-id/${programId}/assets`;
    }


    async getUserApiRoot(): Promise<string> {
        let session = SessionService.EstablishedSession;
        if (session === null) {
            session = await this.getSession();
        }

        return this.getApiRootForUser(session.username);
    }

    getApiRootForUser(username: string): string {
        return this.environmentService.getApiRoot() + '/users/' + username;
    }

    async getApiRootForUserId(user_id?: string): Promise<string> {
        if (!user_id) {
            let session = SessionService.EstablishedSession;
            if (!session) {
                session = await this.getSession();
            }
            user_id = session.user_id;
        }
        return this.environmentService.getApiRoot() + '/users/id/' + user_id;
    }

    async getApiRootForUserIdNew(user_id?: string): Promise<string> {
        if (!user_id) {
            let session = SessionService.EstablishedSession;
            if (!session) {
                session = await this.getSession();
            }
            user_id = session.user_id;
        }
        return this.environmentService.getApiRoot() + '/users/by-id/' + user_id;
    }

    async getUserAvatarUrl(): Promise<string> {
        const root = await this.getApiRootForUserIdNew();

        return `${root}/picture`;
    }

    getUpdateUserAvatarUrl(): Promise<string> {
        return this.getUserAvatarUrl();
    }

    storeToken(token: string) {
        const storage = this.browser.window.localStorage;

        storage.setItem('session.service.token', token);
        this.cookies.put(this.COOKIE_TOKEN_KEY, token);
    }

    removeToken() {
        const storage = this.browser.window.localStorage;
        storage.removeItem('session.service.token');
        this.cookies.remove(this.COOKIE_TOKEN_KEY);
    }

    getToken(): string | null {
        const storage = this.browser.window.localStorage;

        if (!storage) { // Might happen on SSR
            const auth = this.cookies.get(this.COOKIE_TOKEN_KEY);
            return auth || null;
        }

        const token = storage.getItem('session.service.token');

        return token;
    }

    async updateUserSettings(user_settings_update : { is_advanced?: boolean }): Promise<boolean> {
        const url = (await this.getApiRootForUserId()) + '/settings';
        const response = await (this.http
                                .post(url, JSON.stringify(user_settings_update),
                                      { headers: this.addJsonContentType(this.getAuthHeader()) })
                                .toPromise());

        return (response as { success: boolean }).success;
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
            // Return unactive session
            return Promise.resolve({
                username: null,
                user_id: null,
                active: false,
                tags: {
                    is_admin: false,
                    is_advanced: false,
                    is_in_preview: false,
                }
            });
        }
        else {
            // This is (hopefully) a temporary mechanism to migrate users that
            // only have localStorage authentication to cookie-enabled auth.
            this._duplicateSessionTokenToCookie();
        }

        if (SessionService.EstablishedSession !== null) {
            return Promise.resolve(SessionService.EstablishedSession);
        }

        if (!SessionService.EstablishmentPromise) {
            SessionService.EstablishmentPromise = this.forceUpdateSession();
        }

        return SessionService.EstablishmentPromise;
    }

    login(username: string, password: string): Promise<boolean> {
        return progbar.track(this.http
                             .post(this.getLoginUrl(),
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
                    this.getRegisterUrl(),
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
                                 this.getRegisterValidateUrl(),
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
                                 this.getResetPasswordUrl(),
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
                                 this.getValidatePasswordUpdateUrl(),
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
                                 this.getPasswordUpdateUrl(),
                                 JSON.stringify({
                                     verification_code: verificationCode,
                                     password: password,
                                 }),
                                 { headers }).toPromise()
                             .then(_response => {
                                 return;
                             }));
    }

    async updateUserAvatar(image: File): Promise<void> {
        const formData = new FormData();
        formData.append('file', image);

        const url = await this.getUpdateUserAvatarUrl();

        await this.http.post(url, formData, { headers: this.getAuthHeader() }).toPromise()
    }

    async uploadAsset(file: File, programId: string): Promise<{ success: true, value: string }> {
        const formData = new FormData();
        formData.append('file', file);

        const url = await this.getUploadAssetToProgramIdUrl(programId);

        const result = await this.http.post(url, formData, { headers: this.getAuthHeader() }).toPromise();

        return result as any;
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
        if (SessionService._sessionInfoObserver) {
            SessionService._sessionInfoObserver.next({ session: session });
        }
    }

    private _duplicateSessionTokenToCookie() {
        const token = this.getToken();
        if (token && !(this.cookies.get(this.COOKIE_TOKEN_KEY))) {
            this.cookies.put(this.COOKIE_TOKEN_KEY, token);
        }
    }

    public async forceUpdateSession(): Promise<Session> {
        if (this.getToken() === null) {
            return Promise.resolve(null);
        }

        const response = (await this.http
                          .get(this.getCheckSessionUrl(), { headers: this.getAuthHeader() }).toPromise());

        const check = response as any;
        const session = new Session(check.success,
                                    check.username,
                                    check.user_id,
                                    check.tags);
        this._updateSession(session);

        return session;
    }
}
