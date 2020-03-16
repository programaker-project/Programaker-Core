import { ElementRef, ViewChild, Component } from '@angular/core';
import { Router } from '@angular/router';
import { SessionService, SessionInfoUpdate } from './session.service';
import { Subscription } from 'rxjs';
import { BridgeService, BridgeInfoUpdate } from './bridges/bridge.service';
import { MatSidenav } from '@angular/material/sidenav';

@Component({
    selector: 'app-my-app',
    templateUrl: './app.component.html',
    styleUrls: [
        'app.component.css',
        'libs/css/material-icons.css',
        'libs/css/bootstrap.min.css',
    ],
    providers: [BridgeService, SessionService]
})
export class AppComponent {
    sessionSubscription: Subscription;
    loggedIn: boolean;
    title = 'PrograMaker';
    bridgeCount = 0;

    @ViewChild('sidenav', { static: false })
    private sidenav: ElementRef<MatSidenav>;

    constructor(
        private router: Router,
        private session: SessionService,
        private bridgeService: BridgeService,
    ) {
        this.router = router;
        this.session = session;
        this.loggedIn = false;

        this.session.getSessionMonitor().then((data) => {
            if (data.session !== null) {
                this.loggedIn = data.session.active;
            }
            data.monitor.subscribe({
                next: (update: SessionInfoUpdate) => {
                    this.loggedIn = update.loggedIn;
                },
                error: (error: any) => {
                    console.error("Error reading logs:", error);
                },
                complete: () => {
                    console.error("Session info data stopped")
                }
            });
        });

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
        });

        window.onresize = this.updateVerticalSpaces;
        window.onload = this.updateVerticalSpaces;
    }

    updateVerticalSpaces(): void {
        const height = window.innerHeight;
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

    logout(): void {
        this.session.logout();
        this.loggedIn = false;
        this.gotoLogin();
    }

    goHome(): void {
        this.router.navigate(['/']);
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
