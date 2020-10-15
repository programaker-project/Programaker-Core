import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { GroupCollaboratorEditorComponent } from './group-collaborator-editor.component';

describe('GroupCollaboratorEditorComponent', () => {
  let component: GroupCollaboratorEditorComponent;
  let fixture: ComponentFixture<GroupCollaboratorEditorComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ GroupCollaboratorEditorComponent ]
    })
    .compileComponents();
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
