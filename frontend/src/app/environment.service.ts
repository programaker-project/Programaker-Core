import { Injectable, Inject, PLATFORM_ID } from '@angular/core';
import { environment } from 'environments/environment';
import { isPlatformServer } from '@angular/common';
import { EnvironmentDefinition } from 'environments/environment-definition';

@Injectable({
  providedIn: 'root'
})
export class EnvironmentService {
    environment: EnvironmentDefinition;

    constructor(
        @Inject(PLATFORM_ID) private platformId: Object
    ) {
        this.environment = environment;
    }

    private getApiHost(): string {
        if (this.environment.SSRApiHost && isPlatformServer(this.platformId)) {
            return this.environment.SSRApiHost;
        }

        return this.environment.ApiHost;
    }

    getApiRoot(): string {
        return this.getApiHost() + '/api/v0';
    }

    getBrowserApiHost(): string {
        return this.environment.ApiHost;
    }

    getBrowserApiRoot(): string {
        return this.getBrowserApiHost() + '/api/v0';
    }

    hasYjsWsSyncServer(): boolean {
        return !!this.environment.YjsWsSyncServer;
    }

    getYjsWsSyncServer(): string {
        return this.environment.YjsWsSyncServer;
    }
}
