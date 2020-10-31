import { Injectable, Inject, PLATFORM_ID } from '@angular/core';
import { environment } from 'environments/environment';
import { isPlatformServer } from '@angular/common';

@Injectable({
  providedIn: 'root'
})
export class EnvironmentService {
    environment: {
        contact_mail?: string;
        production: boolean;
        ApiHost: string;
        SSRApiHost?: string;
    };

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
}
