import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
    selector: 'my-app',
    templateUrl: './app.component.html'
})

export class AppComponent {
    title = 'WireUp';

    constructor(
        private router: Router
    ) {
        this.router = router;
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
