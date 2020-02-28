import { Component, Input, OnInit } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { Session } from '../session';
import { SessionService } from '../session.service';


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
                if (session !== null && session.active) {
                    this.router.navigate(['/'], {replaceUrl:true});
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

    goResetPassword(): void {
        this.router.navigate(['/login/reset']);
    }

    doLogIn(): void {
        this.sessionService.login(this.username, this.password).then(success => {
            if (success) {
                this.router.navigate(['/']);
            }
        })
        .catch(reason => {
            this.errorMessage = 'Login error';

            if (reason.error && reason.error.error) {
                if (reason.error.error.type === "user_not_ready") {
                    if (reason.error.error.subtype === "mail_not_verified") {
                        this.errorMessage += ": User not ready, please check the verification mail";
                    }
                    else {
                        this.errorMessage += ": User not available";
                    }
                }
                else {
                    this.errorMessage += '. Error type: ' + reason.error.error.type;
                }
            } else if (reason.status === 400) {
                this.errorMessage += ': Invalid user/password'
            }
            console.log('Error on login:', reason);
        })
    }
}
