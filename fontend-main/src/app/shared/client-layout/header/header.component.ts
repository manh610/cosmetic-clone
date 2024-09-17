import { Component, HostListener, OnInit, VERSION } from '@angular/core';
import { Router } from '@angular/router';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { AuthService } from 'src/app/core/services/auth.service';
import { StorageService } from 'src/app/core/services/auth/storage.service';
import { CartService } from 'src/app/core/services/cart.service';
import { FavoriteService } from 'src/app/core/services/favorite.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { UserService } from 'src/app/core/services/user.service';

@Component({
  selector: 'app-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.scss']
})
export class HeaderComponent implements OnInit{
  version = VERSION;
  accessToken: any;
  user: any;
  isLogin: boolean = false;
  roleId: any = '';

  keyword: any = '';

  constructor(private router: Router,
    public auth: AuthService,
    private storageService: StorageService,
    private _notifi: NotificationService,
    private fnConstants: constants,
    private userService: UserService,
    public cartService: CartService,
    public favoriteService: FavoriteService) {}
  ngOnInit() {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
      this.roleId = this.user.roleId;
      this.isLogin = true;
      const cartUser = this.fnConstants.getFromLocalStorage("cartUser");
      if(!cartUser) {
        this.initCart();
      }else {
        this.cartService.cartTotal = cartUser.totalItem;
      }

      const favoriteUser = this.fnConstants.getFromLocalStorage("favoriteUser");
      if(!favoriteUser) {
        this.initFavorite();
      }else {
        this.favoriteService.favoriteTotal = favoriteUser.totalItem;
      }
    }
  }

  //#region INIT
  initCart() {
    try{
      const sub = this.favoriteService.search(this.user.id).subscribe((res: any) => {
        if(res.status) {
          this.favoriteService.favoriteTotal = res.totalItem;
          localStorage.setItem("favoriteUser", JSON.stringify(res));
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e, notifi.FAIL);
        }
      })
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }

  initFavorite() {
    try{
      const sub = this.cartService.search(this.user.id).subscribe((res: any) => {
        if(res.status) {
          this.cartService.cartTotal = res.totalItem;
          localStorage.setItem("cartUser", JSON.stringify(res));
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e, notifi.FAIL);
        }
      })
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  //#endregion

  //#region ACTION
  routerLogin():void{
    this.router.navigate([routerNav.NAV_LOGIN]);
  }
  routerProfile():void{
    this.router.navigate([routerNav.NAV_PROFILE]);
  }
  routerDiscount():void{
    this.router.navigate([routerNav.NAV_DISCOUNT]);
  }
  routerOrder():void{
    this.router.navigate([routerNav.NAV_ORDER_ME]);
  }

  logout():void{
    this.storageService.saveUser(null);
    localStorage.clear();
    this.isLogin = false;
    this.accessToken = null;
    this._notifi.showSuccess('Đằn xuất thành công', notifi.SUCCESS);
    this.router.navigate(['/']);
    // this.auth.logout().subscribe({
    //   next:res =>{
    //     this.storageService.clean();
    //     this.accessToken = '';
    //     this.isLogin = false;
    //     this._notifi.showSuccess('Đăng xuất', notifi.SUCCESS);
    //   },error: err=>{
    //     this._notifi.showError(err, notifi.FAIL);
    //     console.error(err);
    //   }
    // })
  }
  //#endregion

  filter: any = 'keyword';

  routerSearch() {
    this.router.navigate([routerNav.NAV_SEARCH, this.keyword, this.filter]);
  }
}
