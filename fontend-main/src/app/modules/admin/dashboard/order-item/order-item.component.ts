import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { OrderService } from 'src/app/core/services/order.service';

@Component({
  selector: 'app-order-item',
  templateUrl: './order-item.component.html',
  styleUrls: ['./order-item.component.scss']
})
export class OrderItemComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  order$: any = [];
  currentUser: any;

  constructor(private _notifi: NotificationService,
    private router: Router,
    private fnConstants: constants,
    private orderService: OrderService){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initOrder();
    this.currentUser = this.fnConstants.getFromLocalStorage("currentUser");
  }

  initOrder() {
    try {
      const data = {
        status: 'WAIT_CONFIRM',
        pageIndex: 1,
        pageSize: 100
      }
      const sub = this.orderService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.order$ = res.data;
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex){
      this._notifi.showError(ex, notifi.FAIL);
    }
  }

  routerOrder(data: any) {
    this.router.navigate([routerNav.NAV_ORDER_AD]);
  }

  btnAccept(item: any) {
    const request: any = {
      status: 'PREPARE',
      censor: this.currentUser.id
    }

    try {
      const sub = this.orderService.updateStatus(item.id, request).subscribe((res: any) => {
        this.initOrder();
        this._notifi.showSuccess('Duyệt đơn hàng thành công', notifi.SUCCESS);
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex){
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
}
