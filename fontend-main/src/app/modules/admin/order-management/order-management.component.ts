import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { ModalService } from 'src/app/common/modal/modal.service';
import { constants, notifi } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { OrderService } from 'src/app/core/services/order.service';
import { UserService } from 'src/app/core/services/user.service';
import { OrderFormComponent } from '../../client/user/order-user/order-form/order-form.component';
import { formatDate } from '@angular/common';
import { AddressService } from 'src/app/core/services/address.service';

@Component({
  selector: 'app-order-management',
  templateUrl: './order-management.component.html',
  styleUrls: ['./order-management.component.scss']
})
export class OrderManagementComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];
  public rowsOnPageSet = ['10', '20', '50', '100'];

  user$: any;
  userId: any = '';
  status: any = '';
  rangeDates: any = '';
  orderDateFrom: any = '';
  orderDateTo: any = '';

  province$: any = [];
  district$: any = [];
  ward$: any = [];
  provinceId: any = '';
  districtId: any = '';
  wardId: any = '';


  order$: any = [];
  currentUser: any;
  page: any = 1;
  pageSize: any = 10;
  totalData: any;

  constructor(
    private _notifi: NotificationService,
    private modalService: ModalService,
    private orderService: OrderService,
    private fnConstants: constants,
    private userService: UserService,
    private addressService: AddressService
  ) {}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initOrder();
    this.currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    this.initUser();
    this.initProvinces();
  }

  //#region INIT
  initUser() {
    try{
      let data: any = {
        pageIndex: 1,
        pageSize: 100,
        status: 'ACTIVE'
      }
      const sub = this.userService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.user$ = res.data;
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
  initProvinces() {
    try{
      const sub = this.addressService.getProvinces('').subscribe((res: any) => {
        if(res.status) {
          this.province$ = res.data;
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  initDistricts(code: any) {
    try{
      const data: any = {
        provinceId: code
      }
      const sub = this.addressService.getDistricts(data).subscribe((res: any) => {
        if(res.status) {
          this.district$ = res.data;
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  initWard(code: any) {
    try{
      const data: any = {
        districtId: code
      }
      const sub = this.addressService.getWard(data).subscribe((res: any) => {
        if(res.status) {
          this.ward$ = res.data;
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }

  initOrder() {
    try {
      const data = {
        userId: this.userId ?? '',
        status: this.status ?? '',
        orderDateFrom: this.orderDateFrom ?? '',
        orderDateTo: this.orderDateTo ?? '',
        provinceId: this.provinceId ?? '',
        districtId: this.districtId ?? '',
        wardId: this.wardId ?? '',
        pageIndex: this.page,
        pageSize: this.pageSize
      }
      const sub = this.orderService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.order$ = res.data;
          this.totalData = res.totalItem;
          this.order$.forEach((x: any) => {
            x.isExpand = false;
          })
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
  //#endregion

  //#region ACTION
  expand(data: any) {
    data.isExpand = !data.isExpand;
  }

  updateStatus(data: any) {
    let status = 'WAIT_CONFIRM';
    if(data.status == 'WAIT_CONFIRM') status = 'PREPARE';
    if(data.status == 'PREPARE') status = 'WAIT_DELIVERY';
    if(data.status == 'WAIT_DELIVERY') status = 'DELIVERING';

    const request: any = {
      status: status,
      censor: this.currentUser.id
    }

    try {
      const sub = this.orderService.updateStatus(data.id, request).subscribe((res: any) => {
        this.initOrder();
        this._notifi.showSuccess('Cập nhật trạng thái đơn hàng thành công', notifi.SUCCESS);
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

  openForm(id: string, formType: string) {
    let data = {
      id: id,
      formType: formType
    }
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
  //#endregion

  //#region EVENT
  search(){
    this.page = 1;
    this.initOrder();
  }
  onChange(p: any) {
    this.page = p;
    this.initOrder();
  }
  reload(): void {
    this.page = 1;
    this.pageSize = 10;
    this.status = "";
    this.userId = "";
    this.orderDateFrom = "";
    this.orderDateTo = "";
    this.rangeDates = "";
    this.provinceId = "";
    this.districtId = "";
    this.wardId = "";
    this.initOrder();
  }
  onDateChange(event: any) {
    console.log(event)
    if(event[0]) {
      var startDate = new Date(event[0]).toLocaleString();
      this.orderDateFrom = formatDate(startDate, 'yyyy-MM-dd', 'en-US');
      this.initOrder();
    }
    if(event[1]) {
      var endDate = new Date(event[1]).toLocaleString();
      this.orderDateTo = formatDate(endDate, 'yyyy-MM-dd', 'en-US');
      this.initOrder();
    }
  }
  selectProvince() {
    if(this.provinceId) {
      this.initDistricts(this.provinceId);
    }else {
      this.district$ = [];
    }
    this.initOrder();
  }
  clearProvince() {
    this.districtId = '';
    this.wardId = '';
    this.district$ = [];
    this.ward$ = [];
    this.initOrder();
  }
  selectDistrict() {
    if(this.districtId) {
      this.initWard(this.districtId);
    }else {
      this.ward$ = [];
    }
    this.initOrder();
  }
  clearDistrict() {
    this.wardId = '';
    this.ward$ = [];
    this.initOrder();
  }
  //#endregion

  //#region SELECT
  //#endregion
}
