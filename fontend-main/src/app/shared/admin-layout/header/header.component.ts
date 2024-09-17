import { Component, HostListener, OnInit, VERSION } from '@angular/core';
import { Router } from '@angular/router';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { AuthService } from 'src/app/core/services/auth.service';
import { StorageService } from 'src/app/core/services/auth/storage.service';
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

  constructor(private router: Router,
    public auth: AuthService,
    private storageService: StorageService,
    private _notifi: NotificationService,
    private fnConstants: constants,
    private userService: UserService) {}
  ngOnInit() {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
      this.isLogin = true;
    }
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
  }

}
