import { Injectable } from '@angular/core';
import { Service } from './service';
import { Http } from '@angular/http';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';

@Injectable()
export class ServiceService {
    private getServicesUrl = API.ApiRoot + '/services/';

    constructor(
        private http: Http,
    ) {
        this.http = http;
    }


    getServices(): Promise<Service[]> {
        return (this.http
                .get(this.getServicesUrl)
                .map(response => response.json().map(e => ({ name: e.name, id: e.id })))
                .toPromise());
    }


    getService(id: number): Promise<Service> {
        return this.getServices()
            .then((services) => services.find((service) => service.id === id));
    }
}
