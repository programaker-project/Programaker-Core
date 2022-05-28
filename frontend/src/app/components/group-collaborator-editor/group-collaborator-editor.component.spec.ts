import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { GroupCollaboratorEditorComponent } from './group-collaborator-editor.component';


describe('GroupCollaboratorEditorComponent', () => {
    let component: GroupCollaboratorEditorComponent;
    let fixture: ComponentFixture<GroupCollaboratorEditorComponent>;

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            imports: [
                BrowserCookiesModule.forRoot(),
                RouterTestingModule,
                HttpClientTestingModule,
                MatAutocompleteModule,
            ],
            declarations: [
                GroupCollaboratorEditorComponent
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
        fixture = TestBed.createComponent(GroupCollaboratorEditorComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
