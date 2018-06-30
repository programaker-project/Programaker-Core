import { Component, Input, OnInit } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { Session } from './session';
import { SessionService } from './session.service';
import 'rxjs/add/operator/switchMap';

@Component({
    selector: 'app-my-login-form',
    templateUrl: './login-form.component.html',
    providers: [SessionService]
})

export class LoginFormComponent implements OnInit {
    username: string;
    email: string;
    password: string;
    repeatPassword: string;
    isLogInMode: boolean;
    isSignUpMode: boolean;
    session: Session = null;

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
        this.email = '';
        this.password = '';
        this.repeatPassword = '';
        this.goLogInMode();
    }

    goSignUpMode(): void {
        this.isLogInMode = false;
        this.isSignUpMode = true;
    }

    goLogInMode(): void {
        this.isLogInMode = true;
        this.isSignUpMode = false;
    }

    doLogIn(): void {
        this.sessionService.login(this.username, this.password).then(success => {
            if (success) {
                this.router.navigate(['/']);
            }
        })
        .catch(reason => {
          console.log('Error on login:', reason);
        })
    }

    doSignUp(): void {
        const username = this.username;
        const email = this.email;
        const password = this.password;
        this.sessionService.register(username, email, password).then(success => {
            if (success) {
                this.sessionService.login(username, password)
                .then(loginSuccess => {
                    if (loginSuccess) {
                        this.router.navigate(['/']);
                    }
                })
                .catch(reason => {
                    console.log('Error on login:', reason);
                  })
            }
        })
        .catch(e => {
            console.log('Exception signing up', e);
        })
    }
}
