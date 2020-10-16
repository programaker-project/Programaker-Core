import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { GroupCollaboratorEditorComponent } from './group-collaborator-editor.component';
import { MatAutocompleteModule } from '@angular/material/autocomplete';


describe('GroupCollaboratorEditorComponent', () => {
    let component: GroupCollaboratorEditorComponent;
    let fixture: ComponentFixture<GroupCollaboratorEditorComponent>;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            imports: [
                RouterTestingModule,
                HttpClientTestingModule,
                MatAutocompleteModule,
            ],
            declarations: [
                GroupCollaboratorEditorComponent
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
