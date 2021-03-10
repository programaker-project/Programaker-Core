import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { ProgramEditorSidepanelComponent } from './program-editor-sidepanel.component';
import { ToastrModule } from 'ngx-toastr';


describe('ProgramEditorSidepanelComponent', () => {
    let component: ProgramEditorSidepanelComponent;
    let fixture: ComponentFixture<ProgramEditorSidepanelComponent>;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            imports: [
                BrowserCookiesModule.forRoot(),
                RouterTestingModule,
                HttpClientTestingModule,
                ToastrModule.forRoot(),
            ],
            declarations: [
                ProgramEditorSidepanelComponent,
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
        fixture = TestBed.createComponent(ProgramEditorSidepanelComponent);
        component = fixture.componentInstance;
        component.program = { readonly: true, id: 'test' } as any;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
