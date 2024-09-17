import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { notifi } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { OrderService } from 'src/app/core/services/order.service';

@Component({
  selector: 'app-order-form',
  templateUrl: './order-form.component.html',
  styleUrls: ['./order-form.component.scss']
})
export class OrderFormComponent implements OnInit, OnDestroy{
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();

  formType: any;
  frmId: any;

  reason: any = '';
  order$: any;
  events: any = [];

  private unsubscribe: Subscription[] = [];
  constructor(
    private _notifi: NotificationService,
    private orderService: OrderService
    ){
    }

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.formType = this.dataDialog.formType;
    this.frmId = this.dataDialog.id;
    if(this.formType == 'view') this.getById();
  }

  //#region ACTION
  onSubmit() {
    if(this.formType == 'cancel') {
      const request: any = {
        status: 'CANCELLED',
        reason: this.reason
      }
      try {
        const sub = this.orderService.updateStatus(this.frmId, request).subscribe((res: any) => {
          this.saveClicked.emit();
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
    if(this.formType == 'delivered') {
      const request: any = {
        status: 'DELIVERED',
      }
      try {
        const sub = this.orderService.updateStatus(this.frmId, request).subscribe((res: any) => {
          this.saveClicked.emit();
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

  getById() {
    try{
      const sub = this.orderService.detail(this.frmId).subscribe((res: any) => {
        if(res.status) {
          this.order$ = res.data;
          this.events = [
            {status: 'Ngày đặt hàng', date: this.order$.orderDate},
            {status: 'Ngày giao hàng', date: this.order$?.deliveryDate},
            {status: 'Ngày nhận hàng', date: this.order$?.receiptDate}
          ]
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

  cancel(){
    this.cancelClicked.emit()
  }
  //#endregion
}
