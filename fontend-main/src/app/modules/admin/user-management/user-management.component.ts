import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { Observable, Subscription } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { TranslationService } from '../../i18n';
import { UserService } from 'src/app/core/services/user.service';
import { notifi, routerNav } from 'src/app/core/models/constants';
import { ModalService } from 'src/app/common/modal/modal.service';

@Component({
  selector: 'app-user-management',
  templateUrl: './user-management.component.html',
  styleUrls: ['./user-management.component.scss']
})
export class UserManagementComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  //#region VARIABLE
  page: number = 1;
  pageSize: number = 10;
  keyword: string = '';
  totalItem: any;
  status: string = '';
  roleId: string = '';
  userRank: string = '';
  user$: any;

  public rowsOnPageSet = ['10', '20', '50', '100'];
  //#endregion

  ngOnDestroy() {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initUser();
  }

  constructor(
    private _notifi: NotificationService,
    private translate: TranslateService,
    private translation: TranslationService,
    private router: Router,
    private userService: UserService,
    private modalService: ModalService,
  ) {}

  //#region INIT
  initUser() {
    try{
      let data: any = {
        keyword: this.keyword,
        pageIndex: this.page,
        pageSize: this.pageSize,
        status: this.status,
        roleId: this.roleId,
        userRank: this.userRank
      }
      const sub = this.userService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.user$ = res.data;
          this.totalItem = res.totalItem;
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

  delete(data: any): void {
    if(data.id == null){
      return;
    }
    try{
      this.userService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa người dùng ' +data.username+ ' thành công', notifi.SUCCESS);
        this.initUser();
    },error =>{
      for (let e of error.error.errors) {
        this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
      }
    }, () => {
    })
    }catch(e) {
      this._notifi.showInfo(e, notifi.FAIL);
    }
  }
  block(data: any): void {
    if(data.id == null){
      return;
    }
    try{
      this.userService.block(data.id).subscribe(res => {
        if(data.status == 'ACTIVE')
          this._notifi.showSuccess('Khóa tài khoản người dùng ' +data.username+ ' thành công', notifi.SUCCESS);
        else this._notifi.showSuccess('Mở khóa tài khoản người dùng ' +data.username+ ' thành công', notifi.SUCCESS);
        this.initUser();
    },error =>{
      for (let e of error.error.errors) {
        this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
      }
    }, () => {
    })
    }catch(e) {
      this._notifi.showInfo(e, notifi.FAIL);
    }
  }
  recover(data: any): void {
    if(data.id == null){
      return;
    }
    try{
      this.userService.recover(data.id).subscribe(res => {
        this._notifi.showSuccess('Khôi phục khoản người dùng ' +data.username+ ' thành công', notifi.SUCCESS);
        this.initUser();
    },error =>{
      for (let e of error.error.errors) {
        this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
      }
    }, () => {
    })
    }catch(e) {
      this._notifi.showInfo(e, notifi.FAIL);
    }
  }
  //#endregion

  //#region ACTION
  routerCreate(): void {
    this.router.navigate([routerNav.NAV_USER_ITEM, '0', 'add']);
  }
  routerDetail(userId: string): void {
    this.router.navigate([routerNav.NAV_USER_ITEM, userId, 'detail']);
  }
  routerUpdate(userId: string): void {
    this.router.navigate([routerNav.NAV_USER_ITEM, userId, 'edit']);
  }

  openBlock(data: any) {
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Khóa tài khoản người dùng ' +data.username+ '?','Xác nhận','Hủy');
    modalRef.result.then((result: any) => {
      if (result=='YES'){
        this.block(data);
      }
    })
  }

  openRecover(data: any) {
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Xác nhận khôi phục tài khoản ' +data.username+ '?','Xác nhận','Hủy');
    modalRef.result.then((result: any) => {
      if (result=='YES'){
        this.recover(data);
      }
    })
  }

  openDelete(data: any) {
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Xóa tài khoản người dùng ' +data.username+ '?','Xác nhận','Hủy');
    modalRef.result.then((result: any) => {
      if (result=='YES'){
        this.delete(data);
      }
    })
  }
  //#endregion

  //#region EVENT
  onPageChange(event: any) {
    this.page = event.page + 1;
    this.initUser();
  }
  reload(): void {
    this.page = 1;
    this.pageSize = 10;
    this.keyword = "";
    this.status = "";
    this.roleId = "";
    this.userRank = "";
    this.initUser();
  }
  //#endregion
}
