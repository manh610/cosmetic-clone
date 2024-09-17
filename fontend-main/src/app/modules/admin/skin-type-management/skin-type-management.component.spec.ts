import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SkinTypeManagementComponent } from './skin-type-management.component';

describe('SkinTypeManagementComponent', () => {
  let component: SkinTypeManagementComponent;
  let fixture: ComponentFixture<SkinTypeManagementComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [SkinTypeManagementComponent]
    });
    fixture = TestBed.createComponent(SkinTypeManagementComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
