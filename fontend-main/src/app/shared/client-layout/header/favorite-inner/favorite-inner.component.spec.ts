import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FavoriteInnerComponent } from './favorite-inner.component';

describe('FavoriteInnerComponent', () => {
  let component: FavoriteInnerComponent;
  let fixture: ComponentFixture<FavoriteInnerComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [FavoriteInnerComponent]
    });
    fixture = TestBed.createComponent(FavoriteInnerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
