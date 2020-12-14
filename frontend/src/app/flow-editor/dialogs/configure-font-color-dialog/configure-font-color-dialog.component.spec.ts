import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { ConfigureFontColorDialogComponent } from './configure-font-color-dialog.component';
import { MatDialogModule, MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';


describe('ConfigureFontColorDialogComponent', () => {
    let component: ConfigureFontColorDialogComponent;
    let fixture: ComponentFixture<ConfigureFontColorDialogComponent>;

    const mockDialogRef = {
        close: jasmine.createSpy('close'),
    };
    const mockDialogData = {
        text: 'Test!',
        color: '#000'
    };

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            imports: [
                BrowserCookiesModule.forRoot(),
                RouterTestingModule,
                HttpClientTestingModule,
                MatDialogModule,
            ],
            declarations: [ ConfigureFontColorDialogComponent ],
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
        fixture = TestBed.createComponent(ConfigureFontColorDialogComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
