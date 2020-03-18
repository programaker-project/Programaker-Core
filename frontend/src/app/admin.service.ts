import { Injectable } from '@angular/core';
import {User, UserTags} from './user';

import { SessionService } from './session.service';
import { HttpClient } from '@angular/common/http';
import * as API from './api-config';

export interface UserAdminData extends User {
    status: string;
    email: string;
    registration_time: number;
    last_active_time: number;
}

@Injectable()
export class AdminService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    getListUsersUrl(): string {
        return API.ApiRoot + '/users';
    }

    async listAllUsers(): Promise<UserAdminData[]> {
        const url = this.getListUsersUrl();
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<UserAdminData[]>);
    }
}
