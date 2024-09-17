import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FlipClockComponent } from './flip-clock.component';

describe('FlipClockComponent', () => {
  let component: FlipClockComponent;
  let fixture: ComponentFixture<FlipClockComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [FlipClockComponent]
    });
    fixture = TestBed.createComponent(FlipClockComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
