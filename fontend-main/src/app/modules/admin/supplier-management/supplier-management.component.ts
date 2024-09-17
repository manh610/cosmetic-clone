import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { ModalService } from 'src/app/common/modal/modal.service';
import { notifi } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { SupplierService } from 'src/app/core/services/supplier.service';
import { SupplierItemComponent } from './supplier-item/supplier-item.component';

@Component({
  selector: 'app-supplier-management',
  templateUrl: './supplier-management.component.html',
  styleUrls: ['./supplier-management.component.scss']
})
export class SupplierManagementComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  keyword: string = '';
  status: string = '';
  supplier$: any;

  constructor(
    private _notifi: NotificationService,
    private router: Router,
    private modalService: ModalService,
    private supplierService: SupplierService
  ) {}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initSupplier();
  }

  initSupplier() {
    try{
      let data: any = {
        keyword: this.keyword,
        status: this.status
      }
      const sub = this.supplierService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.supplier$ = res.data;
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

  openForm(data: any, formType: string) {
    let postData = {
      id: data.id,
      formType: formType
    }
    const modalRef = this.modalService.openDialogTemplate(SupplierItemComponent, {
      size: 'lg',
    })
    modalRef.componentInstance.dataDialog = postData;
    if(formType == 'add') {
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initSupplier();
          this._notifi.showSuccess('Thêm mới nhà cung cấp thành công', notifi.SUCCESS);
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
          this.initSupplier();
          this._notifi.showSuccess('Cập nhật nhà cung cấp ' +data.code+' thành công', notifi.SUCCESS);
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
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Bạn chắc chắn xóa nhà cung cấp ' +data.code+ '?','Xác nhận','Hủy');
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
      const sub = this.supplierService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa nhà cung cấp ' +data.code+ ' thành công', notifi.SUCCESS);
        this.initSupplier();
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
}
