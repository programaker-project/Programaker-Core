import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { AuthorizeNewTokenComponent } from './authorize-new-token.component';
import { ToastrModule } from 'ngx-toastr';
import { ActivatedRoute, ParamMap, Params, convertToParamMap } from '@angular/router';
import { ReplaySubject } from 'rxjs';


class ActivatedRouteStub implements Partial<ActivatedRoute> {
    private subject = new ReplaySubject<ParamMap>();

    paramMap = this.subject.asObservable();

    constructor(initialParams?: Params) {
        this.setParamMap(initialParams);
    }

    setParamMap(params?: Params) {
        const paramMap = convertToParamMap(params);
        this.subject.next(paramMap);
    }
}

describe('AuthorizeNewTokenComponent', () => {
    let component: AuthorizeNewTokenComponent;
    let fixture: ComponentFixture<AuthorizeNewTokenComponent>;

    beforeEach(waitForAsync(() => {


        const routeStub = new ActivatedRouteStub({
            scopes: 'list_connections_established list_custom_blocks call_any_bridge',
        });

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
                { provide: ActivatedRoute, useValue: routeStub },
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
