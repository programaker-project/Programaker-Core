import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, waitForAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { AppComponent } from './app.component';
import { MatMenuModule } from '@angular/material/menu';


describe('AppComponent', () => {
    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            imports: [
                BrowserCookiesModule.forRoot(),
                RouterTestingModule,
                HttpClientTestingModule,
                MatMenuModule,
            ],
            declarations: [
                AppComponent
            ],
            providers: [
                {
                    provide: CookiesService,
                    useClass: BrowserCookiesService,
                },
            ]
        }).compileComponents();
    }));

    it('should create the app', waitForAsync(() => {
        const fixture = TestBed.createComponent(AppComponent);
        const app = fixture.debugElement.componentInstance;
        expect(app).toBeTruthy();
    }));

    it('should have an div.app-content', waitForAsync(() => {
        const fixture = TestBed.createComponent(AppComponent);
        fixture.detectChanges();
        const compiled = fixture.debugElement.nativeElement;
        expect(compiled.querySelector('div.app-content')).toBeTruthy();
    }));
});
