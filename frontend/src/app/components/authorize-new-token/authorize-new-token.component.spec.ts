import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { AuthorizeNewTokenComponent } from './authorize-new-token.component';
import { ToastrModule } from 'ngx-toastr';


describe('AuthorizeNewTokenComponent', () => {
    let component: AuthorizeNewTokenComponent;
    let fixture: ComponentFixture<AuthorizeNewTokenComponent>;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            imports: [
                BrowserCookiesModule.forRoot(),
                RouterTestingModule,
                HttpClientTestingModule,
                ToastrModule.forRoot(),
            ],
            declarations: [
                AuthorizeNewTokenComponent,
            ],
            providers: [
                {
                    provide: CookiesService,
                    useClass: BrowserCookiesService,
                },
            ],
        }).compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(AuthorizeNewTokenComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
