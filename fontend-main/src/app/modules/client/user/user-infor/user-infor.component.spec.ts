import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UserInforComponent } from './user-infor.component';

describe('UserInforComponent', () => {
  let component: UserInforComponent;
  let fixture: ComponentFixture<UserInforComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [UserInforComponent]
    });
    fixture = TestBed.createComponent(UserInforComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
