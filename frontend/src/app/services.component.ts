import { Component, OnInit } from '@angular/core';
import { Service } from './service';
import { ServiceService } from './service.service';
import { Router } from '@angular/router';

@Component({
    // moduleId: module.id,
    selector: 'app-my-services',
    templateUrl: './services.component.html',
    providers: [ServiceService]
})

export class ServicesComponent {
    services: Service[] = [];

    constructor(
        private serviceService: ServiceService,
        private router: Router,
    ) {
        this.serviceService = serviceService;
        this.router = router;
    }

    // tslint:disable-next-line:use-life-cycle-interface
    ngOnInit(): void {
        this.serviceService.getAvailableServices()
            .then(services => {
                this.services = services;
            })
            .catch(e => console.log(e));
    }

    addService(): void {
        this.router.navigate(['/services/add']);
    }
}
