import { Component, Input, OnInit } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Session } from '../session';
import { SessionService } from '../session.service';
import 'rxjs/add/operator/switchMap';

@Component({
    selector: 'app-my-verify-code',
    templateUrl: './verify-code.component.html',
    providers: [SessionService],
})

export class VerifyCodeComponent implements OnInit {
    session: Session = null;
    status: 'loading'| 'error' = 'loading';
    verificationCode: string;
    errorMessage: string = '';

    ngOnInit(): void {
        this.route.params
            .switchMap((params: Params) => {
                this.verificationCode = params['verification_code'];
                return this.sessionService.validateRegisterCode(this.verificationCode).catch(err => {
                    this.status = 'error';
                    this.errorMessage = err.message || 'Unknown error';
                    console.warn(err);
                });
            })
            .subscribe(session => {
                console.log("Session:", session);
                if (session) {
                    this.router.navigate(['/']);
                }
            });
    }

    constructor (
        private route: ActivatedRoute,
        private sessionService: SessionService,
        private router: Router,
    ) {
        this.sessionService = sessionService;
        this.router = router;

        this.route = route;
    }
}
