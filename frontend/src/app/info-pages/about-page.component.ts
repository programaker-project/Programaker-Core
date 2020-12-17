import { HttpClient } from '@angular/common/http';
import { AfterViewInit, Component, ElementRef, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { SessionService } from '../session.service';
import { environment } from 'environments/environment';

@Component({
    selector: 'app-about-page',
    templateUrl: './about-page.component.html',
    styleUrls: [
        'about-page.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
    providers: [SessionService, HttpClient]
})
export class AboutPageComponent implements AfterViewInit {
    environment: { [key: string]: any };
    @ViewChild('container') container: ElementRef<HTMLDivElement>;
    loadAbout: Promise<string>;

    constructor (
        private router: Router,
        httpClient: HttpClient,
    ) {
        this.environment = environment;
        this.router = router;


        if (this.environment.aboutPageRender) {
            this.loadAbout = httpClient.get<string>(this.environment.aboutPageRender, { responseType: 'text' as 'json' }).toPromise();
        }
    }
    ngAfterViewInit(): void {
        if (this.loadAbout) {
            this.loadAbout.then(html => this.container.nativeElement.innerHTML = html);
        }
    }

    followCallToAction() {
        this.router.navigate(["/register"]);
    }
}
