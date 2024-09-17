import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { ModalService } from 'src/app/common/modal/modal.service';
import { AddressItemComponent } from '../../user/address/address-item/address-item.component';
import { constants, notifi } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { UserService } from 'src/app/core/services/user.service';
import { AddressService } from 'src/app/core/services/address.service';

@Component({
  selector: 'app-address',
  templateUrl: './address.component.html',
  styleUrls: ['./address.component.scss']
})
export class AddressComponent implements OnInit, OnDestroy{
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();
  private unsubscribe: Subscription[] = [];

  address$: any;
  selectedAddress: any;
  user: any;
  totalData: any;

  constructor(private modalService: ModalService,
    private _notifi: NotificationService,
    private fnConstants: constants,
    private userService: UserService,
    private addressService: AddressService){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.selectedAddress = this.dataDialog?.id;
    this.address$ = this.dataDialog.data;
    this.totalData = this.address$.length;
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
      this.initAddress();
    }
  }

  initAddress() {
    try {
      const sub = this.userService.getAddress(this.user.id).subscribe((res: any) => {
        if(res.status) {
          this.address$ = res.data;
          this.totalData = res.totalItem;
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

  //#region ACTION
  onSubmit() {
    const data = this.address$.filter((x: any) => x.id == this.selectedAddress);
    this.saveClicked.emit(data);
  }
  cancel(){
    this.cancelClicked.emit()
  }
  openDialog(item: any, formType: any) {
    let data = {
      address: item,
      userId: this.user.id,
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
  //#endregion
}
