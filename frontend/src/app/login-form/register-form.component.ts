import { Component, Input, OnInit } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { Session } from '../session';
import { SessionService } from '../session.service';


@Component({
    selector: 'app-my-register-form',
    templateUrl: './register-form.component.html',
    providers: [SessionService],
    styleUrls: [
        'register-form.component.css',
    ],
})

export class RegisterFormComponent implements OnInit {
    username: string;
    email: string;
    password: string;
    repeatedPassword: string;

    validUsername: boolean = false;
    validEmail: boolean = false;
    validPassword: boolean = false;
    validRepeatedPassword: boolean = false;

    userErrorMessage: string;
    emailErrorMessage: string;
    passwordErrorMessage: string;
    repeatedPasswordErrorMessage: string;

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
        this.email = '';
        this.password = '';
        this.repeatedPassword = '';
    }

    goLogInMode(): void {
        this.router.navigate(['/login']);
    }

    validateUsername() {
        this.validUsername = true;
        this.userErrorMessage = "";

        if (this.username.length < 4) {
            this.userErrorMessage = "User name should have at least 4 characters.";
            this.validUsername = false;
        }
    }

    validateEmail() {
        const element = (document.getElementById("registerEmail") as HTMLInputElement);
        this.validEmail = element.validity.valid;
        this.emailErrorMessage = element.validationMessage;
    }

    validatePassword() {
        this.validPassword = true;
        this.passwordErrorMessage = "";

        if (this.password.length < 4) {
            this.passwordErrorMessage = "Password should have at least 4 characters.";
            this.validPassword = false;
        }
    }

    validateRepeatedPassword() {
        this.validRepeatedPassword = true;
        this.repeatedPasswordErrorMessage = "";

        if (this.repeatedPassword != this.password) {
            this.repeatedPasswordErrorMessage = "Password and \"Repeat password\" do not match.";
            this.validRepeatedPassword = false;
        }
    }

    doSignUp(): void {
        const username = this.username;
        const email = this.email;
        const password = this.password;
        this.sessionService.register(username, email, password).then(result => {
            if (result.continue_to_login) {
                this.sessionService.login(username, password)
                .then(loginSuccess => {
                    if (loginSuccess) {
                        this.router.navigate(['/']);
                    }
                })
                .catch(reason => {
                    console.log('Error on login:', reason);
                    this.errorMessage = "Sign up OK, error on login";
                  })
            }
            else {
                this.router.navigate(['/register/wait_for_mail_verification']);
            }
        })
        .catch(reason => {
            this.errorMessage = 'Registration error';

            if (reason.error && reason.error.error) {
                if (reason.error.error.type === "invalid_username") {
                    this.errorMessage += ": Invalid username";
                }
                else if (reason.error.error.type === "colliding_element") {
                    if (reason.error.error.subtype === "username") {
                        this.errorMessage += ": Repeated username";
                    }
                    else if (reason.error.error.subtype === "email") {
                        this.errorMessage += ": Repeated email";
                    }
                    else {
                        this.errorMessage += ": Repeated user data";
                    }
                }
                else {
                    this.errorMessage += '. Error type: ' + reason.error.error.type;
                }
            } else if (reason.status === 400) {
                this.errorMessage += ': Invalid user/password'
            }
            console.log('Registration error:', reason);
        })
    }
}
