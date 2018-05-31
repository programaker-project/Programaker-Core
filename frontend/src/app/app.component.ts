import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
    selector: 'my-app',
    templateUrl: './app.component.html',
    styleUrls: [
        'app.component.css'
    ]
})

export class AppComponent {
    title = 'Auto-mate';

    constructor(
        private router: Router
    ) {
        this.router = router;
    }

    gotoLogin() : void {
        this.router.navigate(['/login']);
    }

    gotoDashboard() : void {
        this.router.navigate(['/dashboard']);
    }

    gotoPrograms() : void {
        this.router.navigate(['/programs']);
    }

    gotoServices() : void {
        this.router.navigate(['/services']);
    }
}
