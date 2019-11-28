import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { SessionService } from '../session.service';
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
    constructor (
        private router: Router,
    ) {
        this.router = router;
    }

    ngOnInit(): void {
    }

    followCallToAction() {
        this.router.navigate(["/register"]);
    }
}
