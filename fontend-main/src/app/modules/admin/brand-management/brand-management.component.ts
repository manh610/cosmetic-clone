import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ModalService } from 'src/app/common/modal/modal.service';
import { notifi } from 'src/app/core/models/constants';
import { BrandService } from 'src/app/core/services/brand.service';
import { BrandItemComponent } from './brand-item/brand-item.component';

@Component({
  selector: 'app-brand-management',
  templateUrl: './brand-management.component.html',
  styleUrls: ['./brand-management.component.scss']
})
export class BrandManagementComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];

  keyword: string = '';
  status: string = '';
  brand$: any;

  constructor(
    private _notifi: NotificationService,
    private router: Router,
    private modalService: ModalService,
    private brandService: BrandService
  ) {}

  ngOnDestroy() {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initBrand();
  }

  initBrand() {
    try{
      let data: any = {
        keyword: this.keyword,
        status: this.status
      }
      const sub = this.brandService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.brand$ = res.data;
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
   openForm(id: string, formType: string) {
    let data = {
      id: id,
      formType: formType
    }
    const modalRef = this.modalService.openDialogTemplate(BrandItemComponent, {
      size: 'lg',
    })
    modalRef.componentInstance.dataDialog = data;
    if(formType == 'add') {
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initBrand();
          this._notifi.showSuccess('Thêm mới thương hiệu thành công', notifi.SUCCESS);
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
          this.initBrand();
          this._notifi.showSuccess('Cập nhật thương hiệu thành công', notifi.SUCCESS);
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
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Bạn chắc chắn xóa thương hiệu ' +data.code+ '?','Xác nhận','Hủy');
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
      const sub = this.brandService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa thương hiệu ' +data.code+ ' thành công', notifi.SUCCESS);
        this.initBrand();
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
  //#endregion
}
