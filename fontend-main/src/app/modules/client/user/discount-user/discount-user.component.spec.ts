import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DiscountUserComponent } from './discount-user.component';

describe('DiscountUserComponent', () => {
  let component: DiscountUserComponent;
  let fixture: ComponentFixture<DiscountUserComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [DiscountUserComponent]
    });
    fixture = TestBed.createComponent(DiscountUserComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
