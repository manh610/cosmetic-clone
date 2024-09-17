import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ModalService } from 'src/app/common/modal/modal.service';
import { notifi } from 'src/app/core/models/constants';
import { SkinTypeService } from 'src/app/core/services/skin-type.service';
import { SkinTypeItemComponent } from './skin-type-item/skin-type-item.component';

@Component({
  selector: 'app-skin-type-management',
  templateUrl: './skin-type-management.component.html',
  styleUrls: ['./skin-type-management.component.scss']
})
export class SkinTypeManagementComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];

  skinType$: any;

  constructor(
    private _notifi: NotificationService,
    private router: Router,
    private modalService: ModalService,
    private skinTypeService: SkinTypeService
  ) {}
  ngOnDestroy() {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initSkinType();
  }

  initSkinType() {
    try{
      const sub = this.skinTypeService.search().subscribe((res: any) => {
        if(res.status) {
          this.skinType$ = res.data;
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
    const modalRef = this.modalService.openDialogTemplate(SkinTypeItemComponent, {
      size: 'lg',
    })
    modalRef.componentInstance.dataDialog = data;
    if(formType == 'add') {
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initSkinType();
          this._notifi.showSuccess('Thêm mới loại da thành công', notifi.SUCCESS);
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
          this.initSkinType();
          this._notifi.showSuccess('Cập nhật loại da thành công', notifi.SUCCESS);
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
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Bạn chắc chắn xóa loại da ' +data.name+ '?','Xác nhận','Hủy');
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
      const sub = this.skinTypeService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa loại da ' +data.name+ ' thành công', notifi.SUCCESS);
        this.initSkinType();
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
