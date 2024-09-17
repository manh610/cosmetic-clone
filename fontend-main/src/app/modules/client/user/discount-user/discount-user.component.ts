import { formatDate } from '@angular/common';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { constants, notifi } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { UserService } from 'src/app/core/services/user.service';

@Component({
  selector: 'app-discount-user',
  templateUrl: './discount-user.component.html',
  styleUrls: ['./discount-user.component.scss']
})
export class DiscountUserComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  user$: any;
  discount$: any = [];

  constructor(
    private _notifi: NotificationService,
    private fnConstants: constants,
    private userService: UserService
  ){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user$ = currentUser;
      this.initDiscount();
    }
  }

  initDiscount() {
    try{
      const data = {
        use: false
      }
      const sub = this.userService.getDiscount(this.user$.id, data).subscribe((res: any) => {
        if(res.status) {
          this.discount$ = res.data;
          this.discount$.forEach((x: any) => {
            const date = new Date(x.startDate);
            const now = new Date();
            if(date >= now) x.next = false; //chua den NSD
            else x.next = true;
            x.startDate = formatDate(x.startDate, 'dd-MM-yyyy', 'en-US');
            x.endDate = formatDate(x.endDate, 'dd-MM-yyyy', 'en-US');
          })
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.en, notifi.FAIL);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
}
