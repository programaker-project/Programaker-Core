
import {switchMap} from 'rxjs/operators';
import { Component, Input, OnInit } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { Session } from '../session';
import { SessionService } from '../session.service';


@Component({
    selector: 'app-my-reset-password-update-password',
    templateUrl: './reset-password-update-password.component.html',
    providers: [SessionService],
})

export class ResetPasswordUpdatePasswordComponent implements OnInit {
    session: Session = null;
    status: 'validatedToken' | 'error' | 'validating'= 'validating';
    verificationCode: string;

    errorMessage = '';
    infoMessage = '';

    validPassword: boolean;
    validRepeatedPassword: boolean;
    validatedToken: boolean;

    password: string;
    repeatedPassword: string;

    passwordErrorMessage: string;
    repeatedPasswordErrorMessage: string;

    ngOnInit(): void {
        this.route.params.pipe(
            switchMap((params: Params) => {
                this.verificationCode = params['reset_verification_code'];
                return this.sessionService.validatePasswordUpdateCode(this.verificationCode).catch(err => {
                    this.status = 'error';
                    this.errorMessage = err.message || 'Unknown error';
                    console.warn(err);
                });
            }))
            .subscribe(() => {
                this.status = 'validatedToken';
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

    resetPassword() {
        this.validatePassword();
        this.validateRepeatedPassword();

        if (this.validPassword && this.validRepeatedPassword) {
            this.sessionService.resetPasswordUpdate(this.verificationCode, this.password).then(() => {
                this.router.navigate(['/login']);
            }).catch((err) => {
                this.errorMessage = err.message || 'Unknown error';
                console.warn(err);
            });
        }
    }
}
