import { ComponentFixture, TestBed } from '@angular/core/testing';

import { OptionAttributeComponent } from './option-attribute.component';

describe('OptionAttributeComponent', () => {
  let component: OptionAttributeComponent;
  let fixture: ComponentFixture<OptionAttributeComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [OptionAttributeComponent]
    });
    fixture = TestBed.createComponent(OptionAttributeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
