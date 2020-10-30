import { HttpClient } from '@angular/common/http';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BrowserDynamicTestingModule } from '@angular/platform-browser-dynamic/testing';
import { NoopAnimationsModule } from '@angular/platform-browser/animations';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { SelectProgrammingModelDialogComponent } from './select-programming-model-dialog.component';


describe('SelectProgrammingModelDialogComponent', () => {
    let component: SelectProgrammingModelDialogComponent;
    let fixture: ComponentFixture<SelectProgrammingModelDialogComponent>;

    const mockDialogRef = {
        close: jasmine.createSpy('close')
    };

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [ SelectProgrammingModelDialogComponent ],
            imports: [
                BrowserCookiesModule.forRoot(),
                MatDialogModule,
                NoopAnimationsModule,
            ],
            providers: [
                {
                    provide: CookiesService,
                    useClass: BrowserCookiesService,
                },
                {
                    provide: HttpClient,
                    useValue: {}
                },
                {
                    provide: MatDialogRef,
                    useValue: mockDialogRef
                },
                {
                    provide: MAT_DIALOG_DATA,
                    useValue: [] // Add any data you wish to test if it is passed/used correctly
                }
            ]
        });


        TestBed.overrideModule(BrowserDynamicTestingModule, {
            set: {
                entryComponents: [ SelectProgrammingModelDialogComponent ]
            }
        });

        TestBed.compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(SelectProgrammingModelDialogComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();

    });

    it('should create', () => {
        expect(component).toBeTruthy();

    });
});
