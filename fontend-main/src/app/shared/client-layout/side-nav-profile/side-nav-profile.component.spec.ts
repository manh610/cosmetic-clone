import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SideNavProfileComponent } from './side-nav-profile.component';

describe('SideNavProfileComponent', () => {
  let component: SideNavProfileComponent;
  let fixture: ComponentFixture<SideNavProfileComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [SideNavProfileComponent]
    });
    fixture = TestBed.createComponent(SideNavProfileComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
