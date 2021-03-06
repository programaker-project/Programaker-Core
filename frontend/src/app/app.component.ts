import { ElementRef, ViewChild, Component, Inject, PLATFORM_ID } from '@angular/core';
import { Router } from '@angular/router';
import { SessionService, SessionInfoUpdate } from './session.service';
import { Session } from './session';
import { Subscription } from 'rxjs';
import { getUserPictureUrl } from './utils';
import { BrowserService } from './browser.service';
import { EnvironmentService } from './environment.service';

@Component({
    selector: 'app-my-app',
    templateUrl: './app.component.html',
    styleUrls: [
        'app.component.css',
        'libs/css/material-icons.css',
        'libs/css/bootstrap.min.css',
    ],
    providers: [SessionService,]
})
export class AppComponent {
    sessionSubscription: Subscription;
    title = 'PrograMaker';
    session: Session;

    readonly _getUserPicture: (userId: string) => string;

    constructor(
        private router: Router,
        private browser: BrowserService,
        private environmentService: EnvironmentService,

        public sessionService: SessionService,
    ) {
        this._getUserPicture = getUserPictureUrl.bind(this, environmentService);

        this.router = router;
        this.session = { active: false } as any;

        this.sessionService.getSessionMonitor().then((data) => {
            if (data.session !== null) {
                this.session = data.session;
            }
            data.monitor?.subscribe({
                next: (update: SessionInfoUpdate) => {
                    this.session = update.session;
                },
                error: (error: any) => {
                    console.error("Error reading logs:", error);
                },
                complete: () => {
                    console.error("Session info data stopped")
                }
            });
        }).catch(err => {
            console.warn('Error monitoring session:', err);
        });

        this.browser.window.onresize = this.updateVerticalSpaces.bind(this);
        this.browser.window.onload = this.updateVerticalSpaces.bind(this);
    }

    ngOnInit() {
    }

    updateVerticalSpaces(): void {
        const height = this.browser.window.innerHeight;
        const higherPart = document.getElementById('main-toolbar') as HTMLElement;
        const lowerPart = document.getElementById('app-content') as HTMLElement;

        lowerPart.style.minHeight = (height - higherPart.clientHeight) + 'px';
    }

    logout(): void {
        this.sessionService.logout();
        this.router.navigate(['/login']);
    }
}
