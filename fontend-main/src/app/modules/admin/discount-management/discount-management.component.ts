import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { ModalService } from 'src/app/common/modal/modal.service';
import { notifi } from 'src/app/core/models/constants';
import { DiscountService } from 'src/app/core/services/discount.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { DiscountItemComponent } from './discount-item/discount-item.component';
import { DiscountSendComponent } from './discount-send/discount-send.component';

@Component({
  selector: 'app-discount-management',
  templateUrl: './discount-management.component.html',
  styleUrls: ['./discount-management.component.scss']
})
export class DiscountManagementComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  keyword: string = '';
  discountType: string = '';
  pageIndex: number = 1;
  pageSize: number = 10;
  discount$: any;

  constructor(
    private _notifi: NotificationService,
    private router: Router,
    private modalService: ModalService,
    private discountService: DiscountService
  ) {}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initDiscount();
  }

  initDiscount() {
    try{
      let data: any = {
        keyword: this.keyword,
        discountType: this.discountType,
        pageIndex: this.pageIndex,
        pageSize: this.pageSize
      }
      const sub = this.discountService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.discount$ = res.data;
          this.discount$.forEach((x: any) => {
            const date = new Date(x.endDate);
            const now = new Date();
            if(date >= now) x.expired = false;
            else x.expired = true;
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

  //#region ACTION
  openForm(data: any, formType: string) {
    let postData = {
      id: data.id,
      formType: formType
    }
    const modalRef = this.modalService.openDialogTemplate(DiscountItemComponent, {
      size: 'lg',
    })
    modalRef.componentInstance.dataDialog = postData;
    if(formType == 'add') {
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initDiscount();
          this._notifi.showSuccess('Thêm mới mã giảm giá thành công', notifi.SUCCESS);
          modalRef.close();
        }
        );
        modalRef.componentInstance.cancelClicked.subscribe(() =>{
          modalRef.close();
        })
      }catch(e) {
        this._notifi.showInfo(e, notifi.FAIL);
      }
    }else {
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initDiscount();
          this._notifi.showSuccess('Cập nhật mã giảm giá ' +data.code+ ' thành công', notifi.SUCCESS);
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
  }
  openDelete(data: any) {
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Bạn chắc chắn xóa mã giảm giá ' +data.code+ '?','Xác nhận','Hủy');
    modalRef.result.then((result: any) => {
      if (result=='YES'){
        this.delete(data);
      }
    })
  }
  delete(data: any): void {
    if(data.id == null){
      return;
    }
    try{
      const sub = this.discountService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa mã giảm giá ' +data.code+ ' thành công', notifi.SUCCESS);
        this.initDiscount();
    },error =>{
      for (let e of error.error.errors) {
        this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
      }
    }, () => {
    })
    this.unsubscribe.push(sub);
    }catch(e) {
      this._notifi.showInfo(e, notifi.FAIL);
    }
  }
  openSend(data: any) {
    const modalRef = this.modalService.openDialogTemplate(DiscountSendComponent, {
      size: 'xl',
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
}
