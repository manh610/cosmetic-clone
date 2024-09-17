import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DiscountVariantComponent } from './discount-variant.component';

describe('DiscountVariantComponent', () => {
  let component: DiscountVariantComponent;
  let fixture: ComponentFixture<DiscountVariantComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [DiscountVariantComponent]
    });
    fixture = TestBed.createComponent(DiscountVariantComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
