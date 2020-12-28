import { HttpClient } from '@angular/common/http';
import { AfterViewInit, Component, ElementRef, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { environment } from 'environments/environment';
import { SessionService } from '../session.service';

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

    constructor (
        private router: Router,
        private route: ActivatedRoute,
    ) {
        this.environment = environment;
    }

    ngAfterViewInit(): void {
        this.route.data
            .subscribe((data: {renderedAbout: string }) => {
                if (data.renderedAbout) {
                    this.container.nativeElement.innerHTML = data.renderedAbout;
                }
            });
    }

    followCallToAction() {
        this.router.navigate(["/register"]);
    }
}
