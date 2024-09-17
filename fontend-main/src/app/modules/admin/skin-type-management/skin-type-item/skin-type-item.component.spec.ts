import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SkinTypeItemComponent } from './skin-type-item.component';

describe('SkinTypeItemComponent', () => {
  let component: SkinTypeItemComponent;
  let fixture: ComponentFixture<SkinTypeItemComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [SkinTypeItemComponent]
    });
    fixture = TestBed.createComponent(SkinTypeItemComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
