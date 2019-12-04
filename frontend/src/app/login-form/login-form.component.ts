import { Component, Input, OnInit } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { Session } from '../session';
import { SessionService } from '../session.service';
import 'rxjs/add/operator/switchMap';

@Component({
    selector: 'app-my-login-form',
    templateUrl: './login-form.component.html',
    providers: [SessionService]
})

export class LoginFormComponent implements OnInit {
    username: string;
    password: string;
    session: Session = null;
    errorMessage = '';

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (session.active) {
                    this.router.navigate(['/']);
                }
            });
    }

    goBack(): void {
        this.location.back();
    }

    constructor (
        private route: ActivatedRoute,
        private location: Location,
        private sessionService: SessionService,
        private router: Router,
    ) {
        this.sessionService = sessionService;
        this.router = router;

        this.route = route;
        this.location = location;
        this.username = '';
        this.password = '';
    }

    goSignUpMode(): void {
        this.router.navigate(['/register']);
    }

    doLogIn(): void {
        this.sessionService.login(this.username, this.password).then(success => {
            if (success) {
                this.router.navigate(['/']);
            }
        })
        .catch(reason => {
            this.errorMessage = 'Login error';
            if (reason.status === 400) {
                this.errorMessage += ': Invalid user/password'
            }
            console.log('Error on login:', reason);
        })
    }
}
