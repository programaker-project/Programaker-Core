import { ElementRef, ViewChild, Component, Inject, PLATFORM_ID } from '@angular/core';
import { Router } from '@angular/router';
import { SessionService, SessionInfoUpdate } from './session.service';
import { Session } from './session';
import { Subscription } from 'rxjs';
import { BridgeService, BridgeInfoUpdate } from './bridges/bridge.service';
import { MatSidenav } from '@angular/material/sidenav';
import { getUserPictureUrl } from './utils';
import { BrowserService } from './browser.service';
import { isPlatformBrowser } from '@angular/common';

@Component({
    selector: 'app-my-app',
    templateUrl: './app.component.html',
    styleUrls: [
        'app.component.css',
        'libs/css/material-icons.css',
        'libs/css/bootstrap.min.css',
    ],
    providers: [BridgeService, SessionService,]
})
export class AppComponent {
    sessionSubscription: Subscription;
    title = 'PrograMaker';
    bridgeCount = 0;
    session: Session;

    @ViewChild('sidenav', { static: false })
    private sidenav: ElementRef<MatSidenav>;
    _getUserPicture = getUserPictureUrl;

    constructor(
        private router: Router,
        private browser: BrowserService,

        public sessionService: SessionService,
        private bridgeService: BridgeService,
    ) {
        this.router = router;
        this.session = { active: false } as any;

        this.sessionService.getSessionMonitor().then((data) => {
            if (data.session !== null) {
                this.session = data.session;

                this.bridgeService.listUserBridges().then((data) => {
                    this.bridgeCount = data.bridges.length;

                    data.monitor.subscribe({
                        next: (update: BridgeInfoUpdate) => {
                            this.bridgeCount = update.count;
                        },
                        error: (error: any) => {
                            console.error("Error monitoring bridge count:", error);
                        },
                        complete: () => {
                            console.error("Bridge count data stopped")
                        }
                    });
                }).catch(err => {
                    console.warn('Error listing bridges:', err);
                });
            }
            data.monitor.subscribe({
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
        const lowerPart = document.getElementsByClassName('mat-drawer-container')[0] as HTMLElement;

        lowerPart.style['min-height'] = (height - higherPart.clientHeight) + 'px';
    }

    gotoLogin(): void {
        this.router.navigate(['/login']);
        this.resetSidenavState();
    }

    gotoSettings(): void {
        this.router.navigate(['/settings']);
        this.resetSidenavState();
    }

    gotoAdminPanel(): void {
        this.router.navigate(['/settings/admin']);
        this.resetSidenavState();
    }

    logout(): void {
        this.sessionService.logout();
        this.session = { active: false } as any;
        this.gotoLogin();
    }

    gotoDashboard(): void {
        this.router.navigate(['/dashboard']);
        this.resetSidenavState();
    }

    gotoBridges(): void {
        this.router.navigate(['/bridges']);
        this.resetSidenavState();
    }

    resetSidenavState(): void {
        (this.sidenav as any as MatSidenav).close();
    }
}
