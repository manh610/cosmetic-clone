import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { FavoriteService } from 'src/app/core/services/favorite.service';
import { NotificationService } from 'src/app/core/services/notification.service';

@Component({
  selector: 'app-favorite-user',
  templateUrl: './favorite-user.component.html',
  styleUrls: ['./favorite-user.component.scss']
})
export class FavoriteUserComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  favorite$: any = [];
  user: any;

  constructor(private _notifi: NotificationService,
    private fnConstants: constants,
    private router: Router,
    public favoriteService: FavoriteService) {}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
    }
    this.initFavorite();
  }

  initFavorite() {
    try{
      const sub = this.favoriteService.search(this.user.id).subscribe((res: any) => {
        if(res.status) {
          this.favorite$ = res.data;
          localStorage.setItem("favoriteUser", JSON.stringify(res));
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e, notifi.FAIL);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }

  productItem(product: any) {
    if(product.status == 'STOCK') {
      this.router.navigate([routerNav.NAV_PRODUCT_ITEM_CLIENT, product.id]);
    }
  }
}
