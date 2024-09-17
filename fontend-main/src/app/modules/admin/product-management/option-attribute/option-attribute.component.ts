import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ModalService } from 'src/app/common/modal/modal.service';
import { notifi } from 'src/app/core/models/constants';
import { AttributeService } from 'src/app/core/services/product/option-attribute.service';
import { OptionAttributeItemComponent } from './option-attribute-item/option-attribute-item.component';

@Component({
  selector: 'app-option-attribute',
  templateUrl: './option-attribute.component.html',
  styleUrls: ['./option-attribute.component.scss']
})
export class OptionAttributeComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];

  attribute$: any;
  status: any = '';

  constructor(
    private _notifi: NotificationService,
    private router: Router,
    private modalService: ModalService,
    private attributeService: AttributeService
  ) {}

  ngOnDestroy() {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initAttribute();
  }

  initAttribute() {
    try{
      let data: any = {
        status: this.status
      }
      const sub = this.attributeService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.attribute$ = res.data;
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
    const modalRef = this.modalService.openDialogTemplate(OptionAttributeItemComponent, {
      size: 'lg',
    })
    modalRef.componentInstance.dataDialog = data;
    if(formType == 'add') {
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initAttribute();
          this._notifi.showSuccess('Thêm mới thuộc tính sản phẩm thành công', notifi.SUCCESS);
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
          this.initAttribute();
          this._notifi.showSuccess('Cập nhật thuộc tính sản phẩm thành công', notifi.SUCCESS);
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
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Bạn chắc chắn xóa thuộc tính ' +data.code+ '?','Xác nhận','Hủy');
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
      const sub = this.attributeService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa thuộc tính ' +data.code+ ' thành công', notifi.SUCCESS);
        this.initAttribute();
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
