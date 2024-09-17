import { ComponentFixture, TestBed } from '@angular/core/testing';

import { OptionAttributeItemComponent } from './option-attribute-item.component';

describe('OptionAttributeItemComponent', () => {
  let component: OptionAttributeItemComponent;
  let fixture: ComponentFixture<OptionAttributeItemComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [OptionAttributeItemComponent]
    });
    fixture = TestBed.createComponent(OptionAttributeItemComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
