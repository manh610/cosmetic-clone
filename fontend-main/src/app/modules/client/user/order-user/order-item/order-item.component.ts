import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { ModalService } from 'src/app/common/modal/modal.service';
import { constants, notifi, routerNav, statusOrder } from 'src/app/core/models/constants';
import { DataSharingService } from 'src/app/core/services/data-share.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { OrderService } from 'src/app/core/services/order.service';
import { UserService } from 'src/app/core/services/user.service';
import { OrderFormComponent } from '../order-form/order-form.component';
import { Router } from '@angular/router';
import { ProductService } from 'src/app/core/services/product/product.service';

@Component({
  selector: 'app-order-item',
  templateUrl: './order-item.component.html',
  styleUrls: ['./order-item.component.scss']
})
export class OrderItemComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  user$: any;
  receivedData: any;
  status: any;
  order$: any = [];

  constructor(private router: Router,
    private _notifi: NotificationService,
    private fnConstants: constants,
    private userService: UserService,
    private orderService: OrderService,
    private dataShare: DataSharingService,
    private modalService: ModalService,
    private productService: ProductService,
  ){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user$ = currentUser;
      this.initMatTabData();
      this.initOrder();
    }
  }
  initMatTabData(){
    this.dataShare.getData().subscribe((data: any) => {
      this.receivedData = data;
    })
    if(this.receivedData == null || this.receivedData == 0) this.status = '';
    if(this.receivedData == 1) this.status = statusOrder.WAIT_CONFIRM;
    if(this.receivedData == 2) this.status = statusOrder.PREPARE;
    if(this.receivedData == 3) this.status = statusOrder.WAIT_DELIVERY;
    if(this.receivedData == 4) this.status = statusOrder.DELIVERING;
    if(this.receivedData == 5) this.status = statusOrder.DELIVERED;
    if(this.receivedData == 6) this.status = statusOrder.CANCELLED;
    if(this.receivedData == 7) this.status = statusOrder.RETURN;
  }


  initOrder() {
    try {
      const data = {
        status: this.status
      }
      const sub = this.orderService.getByUser(this.user$.id, data).subscribe((res: any) => {
        if(res.status) {
          this.order$ = res.data;
          this.order$.forEach((x: any) => {
            const orderDate = new Date(x.orderDate);
            orderDate.setDate(orderDate.getDate() + 3);
            x.expectDelivery = orderDate;
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

  routerOrder(data: any) {
    this.router.navigate([routerNav.NAV_ORDER]);
  }

  openForm(id: string, formType: string) {
    let data = {
      id: id,
      formType: formType
    }
    if(formType == 'cancel') {
      const modalRef = this.modalService.openDialogTemplate(OrderFormComponent, {
        size: 'md',
      })
      modalRef.componentInstance.dataDialog = data;
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initOrder();
          this._notifi.showSuccess('Hủy đơn hàng thành công', notifi.SUCCESS);
          modalRef.close();
        }
        );
        modalRef.componentInstance.cancelClicked.subscribe(() =>{
          modalRef.close();
        })
      }catch(e) {
        this._notifi.showInfo(e, notifi.FAIL);
      }
    }else if(formType == 'delivered') {
      const modalRef = this.modalService.openDialogTemplate(OrderFormComponent, {
        size: 'md',
      })
      modalRef.componentInstance.dataDialog = data;
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initOrder();
          this._notifi.showSuccess('Nhận đơn hàng thành công', notifi.SUCCESS);
          modalRef.close();
        }
        );
        modalRef.componentInstance.cancelClicked.subscribe(() =>{
          modalRef.close();
        })
      }catch(e) {
        this._notifi.showInfo(e, notifi.FAIL);
      }
    }
    else {
      const modalRef = this.modalService.openDialogTemplate(OrderFormComponent, {
        size: 'lg',
      })
      modalRef.componentInstance.dataDialog = data;
      try{
        modalRef.componentInstance.cancelClicked.subscribe(() =>{
          modalRef.close();
        })
      }catch(e) {
        this._notifi.showInfo(e, notifi.FAIL);
      }
    }
  }
}
