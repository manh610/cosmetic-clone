import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';
import { Subscription } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { TranslationService } from '../../i18n';
import { ModalService } from 'src/app/common/modal/modal.service';
import { CategoryService } from 'src/app/core/services/category.service';
import { CategoryItemComponent } from './category-item/category-item.component';
import { notifi } from 'src/app/core/models/constants';

@Component({
  selector: 'app-category-management',
  templateUrl: './category-management.component.html',
  styleUrls: ['./category-management.component.scss']
})
export class CategoryManagementComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];
  //#region VARIABLE
  page: number = 1;
  pageSize: number = 10;
  keyword: string = '';
  totalItem: any;

  pageFilter: number = 1;
  pageSizeFilter: number = 10;
  totalItemFilter: any;

  status: any = '';
  parentId: any = '';
  category$: any;
  filterCategory$: any;
  //#endregion

  constructor(
    private _notifi: NotificationService,
    private translate: TranslateService,
    private translation: TranslationService,
    private router: Router,
    private categoryService: CategoryService,
    private modalService: ModalService
  ) {}

  ngOnDestroy() {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initCategory();
    this.initFilterCategory();
  }

  //#region INIT
  initCategory() {
    try{
      let data: any = {
        keyword: this.keyword,
        pageIndex: this.page,
        pageSize: this.pageSize,
        status: this.status,
        parentId: this.parentId == null ? '' : this.parentId
      }
      const sub = this.categoryService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.category$ = res.data;
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
  initFilterCategory() {
    try{
      let data: any = {
        keyword: '',
        pageIndex: this.pageFilter,
        pageSize: this.pageSizeFilter,
        status: 'ACTIVE',
        parentId: ''
      }
      const sub = this.categoryService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.filterCategory$ = res.data;
          this.totalItemFilter = res.totalItem;
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
  openForm(id: string, formType: string) {
    let data = {
      id: id,
      formType: formType
    }
    const modalRef = this.modalService.openDialogTemplate(CategoryItemComponent, {
      size: 'xl',
    })
    modalRef.componentInstance.dataDialog = data;
    if(formType == 'add') {
      try{
        modalRef.componentInstance.saveClicked.subscribe(()  => {
          this.initCategory();
          this.initFilterCategory();
          this._notifi.showSuccess('Thêm mới danh mục thành công', notifi.SUCCESS);
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
          this.initCategory();
          this.initFilterCategory();
          this._notifi.showSuccess('Cập nhật danh mục thành công', notifi.SUCCESS);
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
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Bạn chắc chắn xóa danh mục ' +data.code+ '?','Xác nhận','Hủy');
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
      const sub = this.categoryService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa danh mục ' +data.code+ ' thành công', notifi.SUCCESS);
        this.initCategory();
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

  //#region EVENT
  onPageChange(event: any) {
      this.page = event.page + 1;
      this.initCategory();

  }
  reload(): void {
    this.page = 1;
    this.pageSize = 10;
    this.keyword = "";
    this.status = "";
    this.parentId = "";
    this.initCategory();
  }
  //#endregion
}
