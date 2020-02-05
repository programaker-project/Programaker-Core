import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { SessionService } from '../session.service';
import { environment } from '../../environments/environment';
import 'rxjs/add/operator/switchMap';

@Component({
    selector: 'app-about-page',
    templateUrl: './about-page.component.html',
    styleUrls: [
        'about-page.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
    providers: [SessionService]
})

export class AboutPageComponent implements OnInit {
    environment: { [key: string]: any };

    constructor (
        private router: Router,
    ) {
        this.router = router;
    }

    ngOnInit(): void {
        this.environment = environment;
    }

    followCallToAction() {
        this.router.navigate(["/register"]);
    }
}
