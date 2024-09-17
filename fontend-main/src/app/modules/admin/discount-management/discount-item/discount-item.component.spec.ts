import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DiscountItemComponent } from './discount-item.component';

describe('DiscountItemComponent', () => {
  let component: DiscountItemComponent;
  let fixture: ComponentFixture<DiscountItemComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [DiscountItemComponent]
    });
    fixture = TestBed.createComponent(DiscountItemComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
