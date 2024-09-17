import { formatDate } from '@angular/common';
import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { constants, notifi } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { UserService } from 'src/app/core/services/user.service';

@Component({
  selector: 'app-discount',
  templateUrl: './discount.component.html',
  styleUrls: ['./discount.component.scss']
})
export class DiscountComponent implements OnInit, OnDestroy{
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();
  private unsubscribe: Subscription[] = [];

  user: any;
  discountUser: any;
  discountProduct: any;
  discount$: any = [];
  selectedDiscountUser: any;

  constructor(private _notifi: NotificationService,
    private fnConstants: constants,
    private userService: UserService){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.discountProduct = this.dataDialog.discountProduct;
    if(this.dataDialog.discountUser) {
      this.selectedDiscountUser = this.dataDialog.discountUser.id;
    }
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
      this.initByUser();
      if(this.discountProduct.length > 0) this.discount$ = [...this.discount$, ...this.discountProduct];
    }
  }

  //#region INIT
  initByProduct() {

  }

  initByUser() {
    try{
      const data = {
        use: false
      }
      const sub = this.userService.getDiscount(this.user.id, data).subscribe((res: any) => {
        if(res.status) {
          this.discountUser = res.data;
          this.discountUser.forEach((x: any) => {
            const date = new Date(x.startDate);
            const now = new Date();
            if(date >= now) x.next = false; //chua den NSD
            else x.next = true;
            x.startDate = formatDate(x.startDate, 'dd-MM-yyyy', 'en-US');
            x.endDate = formatDate(x.endDate, 'dd-MM-yyyy', 'en-US');
          })
          this.discount$ = [...this.discount$, ...this.discountUser];
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
  //#endregion

  //#region ACTION
  onSubmit() {
    let data = this.discount$.filter((x: any) => x.id == this.selectedDiscountUser);
    data = data[0];
    this.saveClicked.emit(data);
  }
  cancel(){
    this.cancelClicked.emit()
  }
  //#endregion
}
