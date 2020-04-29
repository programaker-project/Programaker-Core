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

export interface PlatformUserStats {
    count: number;
    registered_last_day: number;
    registered_last_week: number;
    registered_last_month: number;
    logged_last_hour: number;
    logged_last_day: number;
    logged_last_week: number;
    logged_last_month: number;
}

export interface PlatformBridgeStats {
    public_count: number;
    private_count: number;
    connections: number;
    unique_connections: number;
    messages_on_flight: number;
}

export interface PlatformStats {
    active_services: {[key: string]: boolean};
    bot_count: {active: number, workers: number};
    thread_count: {active: number, workers: number};
    monitor_count: {active: number, workers: number};
    service_count: {all: number, public: number};
    user_stats: PlatformUserStats;
    bridge_stats: PlatformBridgeStats;
};

export interface PlatformStatsInfo {
    stats: PlatformStats,
    errors: string[],
};

@Injectable()
export class AdminService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    private getListUsersUrl(): string {
        return API.ApiRoot + '/users';
    }

    private getAdminStatsUrl(): string {
        return API.ApiRoot + '/admin/stats';
    }

    async listAllUsers(): Promise<UserAdminData[]> {
        const url = this.getListUsersUrl();
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<UserAdminData[]>);
    }

    async getStats(): Promise<PlatformStatsInfo> {
        const url = this.getAdminStatsUrl();
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<PlatformStatsInfo>);
    }
}
