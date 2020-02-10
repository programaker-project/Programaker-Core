import { Component, Input, OnInit } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { Session } from '../session';
import { SessionService } from '../session.service';
import 'rxjs/add/operator/switchMap';

@Component({
    selector: 'app-my-reset-password-start',
    templateUrl: './reset-password-start.component.html',
    providers: [SessionService],
    styleUrls: [
        './reset-password-start.component.css',
    ]
})

export class ResetPasswordStartComponent implements OnInit {
    email: string;
    session: Session = null;
    errorMessage = '';
    infoMessage = '';
    emailErrorMessage = '';
    validEmail = false;
    completed = false;

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (session !== null && session.active) {
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
    }

    resetPassword() {
        if (!this.validateEmail(true)) {
            return;
        }

        this.errorMessage = '';

        this.sessionService.requestResetPassword(this.email).then(() => {
            this.infoMessage = 'Password reset requested, check your inbox to continue.';
            this.completed = true;
        }).catch((err: Error) => {
            this.errorMessage = err.message || 'Unknown error';
        })
    }

    validateEmail(updateMessage: boolean): boolean {
        const element = this.getEmailElement();

        let newMessage = '';
        this.validEmail = element.validity.valid;

        if (!this.validEmail) {
            newMessage = element.validationMessage;
        }

        if (updateMessage) {
            this.emailErrorMessage = newMessage;
        }

        return this.validEmail;
    }

    getEmailElement(): HTMLInputElement {
        return document.getElementById('recoveryEmail') as HTMLInputElement;
    }
}
