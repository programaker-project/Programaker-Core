import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { SessionService } from './session.service';
import { Session } from './session';
import { Subscription } from 'rxjs';

@Component({
    selector: 'app-my-app',
    templateUrl: './app.component.html',
    styleUrls: [
        'app.component.css',
        'libs/css/material-icons.css',
        'libs/css/bootstrap.min.css',
    ],
    providers: [SessionService]
})

export class AppComponent {
    sessionSubscription: Subscription;
    username: string;
    loggedIn: boolean;
    title = 'Plaza';

    constructor(
        private router: Router,
        private session: SessionService
    ) {
        this.router = router;
        this.session = session;
        this.loggedIn = false;

        this.session.getSession().then((newSession: Session) => {
            if (newSession !== null) {
                this.loggedIn = newSession.active;
                if (newSession.active) {
                    this.username = newSession.username;
                }
            }
        });
    }

    gotoLogin(): void {
        this.router.navigate(['/login']);
    }

    gotoDashboard(): void {
        this.router.navigate(['/dashboard']);
    }

    gotoPrograms(): void {
        this.router.navigate(['/programs']);
    }

    gotoServices(): void {
        this.router.navigate(['/services']);
    }
}
