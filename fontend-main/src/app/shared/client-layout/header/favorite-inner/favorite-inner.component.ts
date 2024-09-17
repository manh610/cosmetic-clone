import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { FavoriteService } from 'src/app/core/services/favorite.service';
import { NotificationService } from 'src/app/core/services/notification.service';

@Component({
  selector: 'app-favorite-inner',
  templateUrl: './favorite-inner.component.html',
  styleUrls: ['./favorite-inner.component.scss']
})
export class FavoriteInnerComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  favorite$: any = [];
  user: any;

  constructor(private _notifi: NotificationService,
    private fnConstants: constants,
    private route: ActivatedRoute,
    private router: Router,
    public favoriteService: FavoriteService) {}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
      const favoriteUser = this.fnConstants.getFromLocalStorage("favoriteUser");
      if(!favoriteUser) {
        this.initfavorite();
      }else {
        this.favorite$ = favoriteUser.data;
        this.favoriteService.favoriteTotal = favoriteUser.totalItem;
      }
    }
  }

  initfavorite() {
    try{
      const sub = this.favoriteService.search(this.user.id).subscribe((res: any) => {
        if(res.status) {
          this.favorite$ = res.data;
          this.favoriteService.favoriteTotal = res.totalItem;
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

  routerFavorite() {
    this.router.navigate([routerNav.NAV_FAVORITE]);
  }
}
