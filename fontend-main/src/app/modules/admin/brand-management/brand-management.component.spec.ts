import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BrandManagementComponent } from './brand-management.component';

describe('BrandManagementComponent', () => {
  let component: BrandManagementComponent;
  let fixture: ComponentFixture<BrandManagementComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [BrandManagementComponent]
    });
    fixture = TestBed.createComponent(BrandManagementComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
