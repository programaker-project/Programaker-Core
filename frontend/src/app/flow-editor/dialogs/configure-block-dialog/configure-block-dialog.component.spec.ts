import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { ConfigureBlockDialogComponent } from './configure-block-dialog.component';
import { MatDialogModule, MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';


describe('ConfigureBlockDialogComponent', () => {
    let component: ConfigureBlockDialogComponent;
    let fixture: ComponentFixture<ConfigureBlockDialogComponent>;

    const mockDialogRef = {
        close: jasmine.createSpy('close'),
    };
    const mockDialogData = {
        block: {
            isBackgroundConfigurable: false,
            getCurrentConfiguration: () => { return {}; },
        }
    };

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            imports: [
                BrowserCookiesModule.forRoot(),
                RouterTestingModule,
                HttpClientTestingModule,
                MatDialogModule,
            ],
            declarations: [ ConfigureBlockDialogComponent ],
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
        fixture = TestBed.createComponent(ConfigureBlockDialogComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
