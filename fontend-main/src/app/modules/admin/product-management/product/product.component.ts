import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ModalService } from 'src/app/common/modal/modal.service';
import { notifi, routerNav } from 'src/app/core/models/constants';
import { ProductService } from 'src/app/core/services/product/product.service';
import { ProductItemComponent } from './product-item/product-item.component';
import { BrandService } from 'src/app/core/services/brand.service';
import { SkinTypeService } from 'src/app/core/services/skin-type.service';
import { CategoryService } from 'src/app/core/services/category.service';

@Component({
  selector: 'app-product',
  templateUrl: './product.component.html',
  styleUrls: ['./product.component.scss']
})
export class ProductComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];

  product$: any;
  brand$: any;
  skinType$: any;
  category$: any;
  keyword: any = '';
  status: any = '';
  brandId: any = '';
  categoryId: any = '';
  skinTypeId: any = '';
  pageP: number = 1;
  pageSizeP: number = 10;
  totalItemP: any;

  constructor(
    private _notifi: NotificationService,
    private router: Router,
    private modalService: ModalService,
    private productService: ProductService,
    private brandService: BrandService,
    private skinTypeService: SkinTypeService,
    private categoryService: CategoryService
  ) {}

  ngOnDestroy() {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initProduct();
    this.initBrand();
    this.initSkinType();
    this.initCategory();
  }

  //#region INIT
  initProduct() {
    try{
      let data: any = {
        keyword: this.keyword,
        status: this.status,
        brandId: this.brandId ?? '',
        categoryId: this.categoryId ?? '',
        skinTypeId: this.skinTypeId ?? '',
        pageIndex: this.pageP,
        pageSize: this.pageSizeP
      }
      const sub = this.productService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.product$ = res.data;
          this.totalItemP = res.totalItem;
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
  initBrand() {
    try{
      const data: any = {
        keyword: '',
        status: 'ACTIVE'
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
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
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
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  initCategory() {
    try {
      const data: any = {
        keyword: '',
        status: 'ACTIVE',
        pageIndex: 1,
        pageSize: 30
      }
      const sub = this.categoryService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.category$ = res.data;
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
  //#endregion

  //#region ACTION
  routerCreate(): void {
    this.router.navigate([routerNav.NAV_PRODUCT_ITEM, '0', 'add']);
  }
  routerUpdate(prId: string): void {
    this.router.navigate([routerNav.NAV_PRODUCT_ITEM, prId, 'edit']);
  }
  openDelete(data: any) {
    const modalRef = this.modalService.openDialogConfirm('Cảnh báo','Bạn chắc chắn xóa sản phẩm ' +data.code+ '?','Xác nhận','Hủy');
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
      const sub = this.productService.delete(data.id).subscribe(res => {
        this._notifi.showSuccess('Xóa sản phẩm ' +data.code+ ' thành công', notifi.SUCCESS);
        this.initProduct();
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
    this.pageP = event.page + 1;
    this.initProduct();

  }
  reload(): void {
    this.keyword = '',
    this.status = '',
    this.brandId = '',
    this.categoryId = '',
    this.skinTypeId = '',
    this.pageP = 1,
    this.pageSizeP = 10,
    this.initProduct();
  }
  //#endregion
}
