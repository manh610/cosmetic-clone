import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DiscountSendComponent } from './discount-send.component';

describe('DiscountSendComponent', () => {
  let component: DiscountSendComponent;
  let fixture: ComponentFixture<DiscountSendComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [DiscountSendComponent]
    });
    fixture = TestBed.createComponent(DiscountSendComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
