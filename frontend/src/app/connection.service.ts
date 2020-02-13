import {map} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Service, AvailableService, ServiceEnableHowTo } from './service';
import * as API from './api-config';


import { SessionService } from './session.service';
import { ServiceService } from './service.service';
import { HttpClient } from '@angular/common/http';
import { ContentType } from './content-type';

/**
 * This is a Mock to experiment with connection-management flows.
 *
 */

@Injectable()
export class ConnectionService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
        private serviceService: ServiceService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
        this.serviceService = serviceService;
    }


    async getAvailableBridges(): Promise<AvailableService[]> {
        const bridges = await this.serviceService.getAvailableServices();

        return bridges;
    }
}
