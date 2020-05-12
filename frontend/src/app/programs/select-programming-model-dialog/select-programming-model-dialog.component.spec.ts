import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { SelectProgrammingModelDialogComponent } from './select-programming-model-dialog.component';

describe('SelectProgrammingModelDialogComponent', () => {
  let component: SelectProgrammingModelDialogComponent;
  let fixture: ComponentFixture<SelectProgrammingModelDialogComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ SelectProgrammingModelDialogComponent ]
    })
    .compileComponents();
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
