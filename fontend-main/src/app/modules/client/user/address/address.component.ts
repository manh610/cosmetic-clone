import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { ModalService } from 'src/app/common/modal/modal.service';
import { constants, notifi } from 'src/app/core/models/constants';
import { AddressService } from 'src/app/core/services/address.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { UserService } from 'src/app/core/services/user.service';
import { AddressItemComponent } from './address-item/address-item.component';

@Component({
  selector: 'app-address',
  templateUrl: './address.component.html',
  styleUrls: ['./address.component.scss']
})
export class AddressComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  user$: any;
  address$: any = [];
  totalData = 0;

  constructor(
    private _notifi: NotificationService,
    private fnConstants: constants,
    private userService: UserService,
    private addressService: AddressService,
    private modalService: ModalService
  ){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user$ = currentUser;
      this.initAddress();
    }
  }

  initAddress() {
    try {
      const sub = this.userService.getAddress(this.user$.id).subscribe((res: any) => {
        if(res.status) {
          this.address$ = res.data;
          this.totalData = res.totalItem;
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

  //#region CRUD
  changeDefault(data: any) {
    if(!data.default) {
      try {
        const sub = this.addressService.changeDefault(this.user$.id, data.id).subscribe((res: any) => {
          if(res.status) {
            this.initAddress();
            this._notifi.showSuccess("Thiết lập mặc định thành công", notifi.SUCCESS);
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            console.error(e.code + '-' + e.message.en, notifi.FAIL);
          }
        })
        this.unsubscribe.push(sub);
      }catch(ex) {
        this._notifi.showError(ex, notifi.FAIL);
      }
    }
  }

  openDialog(item: any, formType: any) {
    let data = {
      address: item,
      userId: this.user$.id,
      formType: formType
    }
    const modalRef = this.modalService.openDialogTemplate(AddressItemComponent, {
      size: 'lg',
    })
    modalRef.componentInstance.dataDialog = data;
    if(formType == 'add') {
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initAddress();
          this._notifi.showSuccess('Thêm mới địa chỉ thành công', notifi.SUCCESS);
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
          this.initAddress();
          this._notifi.showSuccess('Cập nhật địa chỉ thành công', notifi.SUCCESS);
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
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Bạn chắc chắn xóa địa chỉ này?','Xác nhận','Hủy');
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
    if(data.default) {
      this._notifi.showInfo("Không thể xóa địa chỉ mặc định", notifi.INFO);
      return;
    }
    try{
      const sub = this.addressService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa địa chỉ thành công', notifi.SUCCESS);
        this.initAddress();
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
