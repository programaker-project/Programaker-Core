import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { SessionService } from '../session.service';


@Component({
    selector: 'app-home-redirect',
    templateUrl: './home-redirect.component.html',
    providers: [SessionService]
})

export class HomeRedirectComponent implements OnInit {
    constructor (
        private sessionService: SessionService,
        private router: Router,
    ) {
        this.sessionService = sessionService;
        this.router = router;
    }

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                if (session.active) {
                    this.router.navigate(['/dashboard']);
                }
                else {
                    this.router.navigate(['/about']);
                }
            }).catch(_err => {
                this.router.navigate(['/about']);
            });
    }
}
