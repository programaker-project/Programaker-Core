import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map } from 'rxjs/operators';
import * as API from './api-config';
import { ContentType } from './content-type';
import { AvailableService, ServiceEnableHowTo } from './service';
import { SessionService } from './session.service';

@Injectable()
export class ServiceService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getListAvailableServicesUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/services/';
    }

    private getListAvailableServicesOnProgramUrl(programId: string): string {
        return `${API.ApiRoot}/programs/by-id/${programId}/services`;
    }

    async getServiceEnableHowToUrl(service: AvailableService) {
        return service.link + '/how-to-enable';
    }

    getServiceEnableOnGroupHowToUrl(service: AvailableService, groupId: string) {
        return `${API.ApiRoot}/services/by-id/${service.id}/how-to-enable`;
    }

    getServiceEnableOnProgramHowToUrl(service: AvailableService, programId: string) {
        return `${API.ApiRoot}/services/by-id/${service.id}/how-to-enable`;
    }

    getServiceRegistryUrl(serviceId: string) {
        return `${API.ApiRoot}/services/by-id/${serviceId}/register`;
    }

    getServiceRegistryUrlOnGroup(service_id: string) {
        return `${API.ApiRoot}/services/by-id/${service_id}/register`;
    }

    getServiceRegistryUrlOnProgram(bridgeId: string, programId: string) {
        return `${API.ApiRoot}/programs/by-id/${programId}/services/by-id/${bridgeId}/register`;
    }

    getAvailableServices(): Promise<AvailableService[]> {
        return this.getListAvailableServicesUrl().then(
            url => this.http.get(url, { headers: this.sessionService.getAuthHeader() })
                .pipe(map(response => response as AvailableService[]))
                .toPromise());
    }

    async getAvailableServicesOnProgram(programId: string): Promise<AvailableService[]> {
        const url = this.getListAvailableServicesOnProgramUrl(programId);

        return await this.http.get(url, { headers: this.sessionService.getAuthHeader() })
            .pipe(map(response => response as AvailableService[]))
            .toPromise();
    }

    async getHowToEnable(service: AvailableService): Promise<ServiceEnableHowTo> {
        const url = await this.getServiceEnableHowToUrl(service);
        return (this.http.get(url, { headers: this.sessionService.getAuthHeader() })
                .toPromise() as Promise<ServiceEnableHowTo>);
    }

    async getHowToEnableOnGroup(service: AvailableService, groupId: string): Promise<ServiceEnableHowTo> {
        const url = this.getServiceEnableOnGroupHowToUrl(service, groupId);
        return (this.http.get(url, { headers: this.sessionService.getAuthHeader(),
                                     params: { group_id: groupId }
                                   })
            .toPromise() as Promise<ServiceEnableHowTo>);
    }

    getHowToEnableOnProgram(service: AvailableService, programId: string): Promise<ServiceEnableHowTo> {
        const url = this.getServiceEnableOnProgramHowToUrl(service, programId);
        return (this.http.get(url, { headers: this.sessionService.getAuthHeader(),
                                     params: { program_id: programId }
                                   })
            .toPromise() as Promise<ServiceEnableHowTo>);
    }

    registerService(data: { [key: string]: string },
                        service_id: string,
                    connection_id: string,
                    params?: { program_id: string, group_id: string } ): Promise<{success: boolean}> {

        if (connection_id) {
            if (data.metadata === undefined) {
                (data as any).metadata = {};
            }
            (data as any).metadata.connection_id = connection_id;
        }

        const url = this.getServiceRegistryUrl(service_id);
        return (this.http.post(
            url, JSON.stringify(data),
            {
                headers: this.sessionService.addContentType(
                    this.sessionService.getAuthHeader(),
                    ContentType.Json),
                params: params,
            }).toPromise()) as Promise<{success: boolean}>;
    }

    async directRegisterService(bridge_id: string): Promise<{success: boolean}> {
        const data = {};

        const url = await this.getServiceRegistryUrl(bridge_id)
        return (this.http.post(
            url, JSON.stringify(data),
            {
                headers: this.sessionService.addContentType(
                    this.sessionService.getAuthHeader(),
                    ContentType.Json),
            }).toPromise()) as Promise<{success: boolean}>;
    }

    async directRegisterServiceOnGroup(bridgeId: string, groupId: string): Promise<{success: boolean}> {
        const data = { };

        const url = this.getServiceRegistryUrlOnGroup(bridgeId)
        return (this.http.post(
            url, JSON.stringify(data),
            {
                headers: this.sessionService.addContentType(
                    this.sessionService.getAuthHeader(),
                    ContentType.Json),
                params: { group_id: groupId }
            }).toPromise()) as Promise<{success: boolean}>;
    }

    async directRegisterServiceOnProgram(bridgeId: string, programId: string): Promise<{ success: boolean; }> {
        const data = { };

        const url = this.getServiceRegistryUrlOnProgram(bridgeId, programId);
        return (this.http.post(
            url, JSON.stringify(data),
            {
                headers: this.sessionService.addContentType(
                    this.sessionService.getAuthHeader(),
                    ContentType.Json),
            }).toPromise()) as Promise<{success: boolean}>;
    }
}
