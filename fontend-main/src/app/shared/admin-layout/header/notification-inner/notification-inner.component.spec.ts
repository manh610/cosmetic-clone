import { ComponentFixture, TestBed } from '@angular/core/testing';

import { NotificationInnerComponent } from './notification-inner.component';

describe('NotificationInnerComponent', () => {
  let component: NotificationInnerComponent;
  let fixture: ComponentFixture<NotificationInnerComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [NotificationInnerComponent]
    });
    fixture = TestBed.createComponent(NotificationInnerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
