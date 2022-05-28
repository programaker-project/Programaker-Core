import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { ConfigureLinkDialogComponent } from './configure-link-dialog.component';
import { MatDialogModule, MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';


describe('ConfigureLinkDialogComponent', () => {
    let component: ConfigureLinkDialogComponent;
    let fixture: ComponentFixture<ConfigureLinkDialogComponent>;

    const mockDialogRef = {
        close: jasmine.createSpy('close'),
    };
    const mockDialogData = {
        text: '',
        link: ''
    };

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            imports: [
                BrowserCookiesModule.forRoot(),
                RouterTestingModule,
                HttpClientTestingModule,
                MatDialogModule,
            ],
            declarations: [ ConfigureLinkDialogComponent ],
            providers: [
                {
                    provide: CookiesService,
                    useClass: BrowserCookiesService,
                },
                {
                    provide: MatDialogRef,
                    useValue: mockDialogRef
                },
                {
                    provide: MAT_DIALOG_DATA,
                    useValue: mockDialogData,
                }
            ]
        }).compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(ConfigureLinkDialogComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
