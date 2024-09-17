import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ValueDetailComponent } from './value-detail.component';

describe('ValueDetailComponent', () => {
  let component: ValueDetailComponent;
  let fixture: ComponentFixture<ValueDetailComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [ValueDetailComponent]
    });
    fixture = TestBed.createComponent(ValueDetailComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
