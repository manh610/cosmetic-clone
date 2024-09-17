import { AfterViewInit, Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { ModalService } from 'src/app/common/modal/modal.service';
import { notifi } from 'src/app/core/models/constants';
import { BrandService } from 'src/app/core/services/brand.service';
import { CategoryService } from 'src/app/core/services/category.service';
import { DiscountService } from 'src/app/core/services/discount.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ProductService } from 'src/app/core/services/product/product.service';
import { SkinTypeService } from 'src/app/core/services/skin-type.service';
import { UserService } from 'src/app/core/services/user.service';
import { DiscountVariantComponent } from './discount-variant/discount-variant.component';

@Component({
  selector: 'app-discount-send',
  templateUrl: './discount-send.component.html',
  styleUrls: ['./discount-send.component.scss']
})
export class DiscountSendComponent implements OnInit, OnDestroy {
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();
  private unsubscribe: Subscription[] = [];

  discount$: any;
  discount: any;

  variantSelected: any = [];

  //#region VARIANT USER
  pageUser: number = 1;
  pageSizeUser: number = 100;
  keywordUser: string = '';
  totalItemUser: any;
  roleId: string = '';
  userRank: string = '';
  user$: any;
  userData: any;

  userSelected: any = [];
  checkedAllUser: boolean = false;
  //#endregion

  //#region VARIANT PRODUCT
  pageProduct: number = 1;
  pageSizeProduct: number = 100;
  keywordProduct: string = '';
  totalItemProduct: any;
  brandId: any = "";
  skinTypeId: any = "";
  categoryId: any = "";
  product$: any;
  productData: any;

  brand$: any;
  skinType$: any;
  category$: any;
  quantityDiscount: any;

  productSelected: any = [];
  checkedAllProduct: boolean = false;
  //#endregion

  constructor(
    private _notifi: NotificationService,
    private router: Router,
    private _modalService: ModalService,
    private userService: UserService,
    private productService: ProductService,
    private discountService: DiscountService,
    private brandService: BrandService,
    private skinTypeService: SkinTypeService,
    private categoryService: CategoryService
  ) {
  }

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.discount$ = this.dataDialog;
    this.initDiscount();
    this.initBrand();
    this.initSkinType();
    this.initCategory();
  }

  //#region INIT
  initDiscount() {
    try{
      const sub = this.discountService.detail(this.discount$.id).subscribe((res: any) => {
        if(res.status) {
          this.discount = res.data;
          this.initUser();
          this.initProduct();
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
  initUser() {
    try{
      let data: any = {
        keyword: this.keywordUser,
        pageIndex: this.pageUser,
        pageSize: this.pageSizeUser,
        status: 'ACTIVE',
        roleId: this.roleId,
        userRank: this.userRank
      }
      const sub = this.userService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.user$ = res.data;
          this.userData = this.user$.filter((x: any) => x.id);
          if(this.discount.users) {
            this.user$.forEach((item: any) => {
              this.checkUserExist(item);
            })
          }
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
  initProduct() {
    try{
      let data: any = {
        keyword: this.keywordProduct,
        pageIndex: this.pageProduct,
        pageSize: this.pageSizeProduct,
        status: 'STOCK',
        brandId: this.brandId ?? '',
        skinTypeId: this.skinTypeId ?? '',
        categoryId: this.categoryId ?? '',
        isDate: false
      }
      const sub = this.productService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.product$ = res.data;
          this.productData = this.product$.filter((x: any) => x.id);
          this.totalItemProduct = res.totalItem;
          if(this.discount.products) {
            this.product$.forEach((item: any) => {
              this.checkProductExist(item);
            })
          }
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

  //#region ACTION USER
  checkUserExist(data: any) {
    this.discount.users.filter((m: any) => {
      if (m.id == data.id) {
        data.checked = true;
        data.disabled = true;
      }
    })
  }
  onCheckboxChangeUser(data: any) {
    if(this.checkedAllUser) {
      this.checkedAllUser = false;
      this.userSelected.forEach((x: any) => {
        if(x.id == data.id) {
          data.checked = false;
          const index = this.userSelected.indexOf(x);
          if (index >= 0) {
            this.userSelected.splice(index, 1);
          }
        }
      })
    }else {
      let check: boolean = true;
      this.userSelected.forEach((x: any) => {
        if(x.id == data.id) {
          check = false;
          data.checked = false;
          const index = this.userSelected.indexOf(x);
          if (index >= 0) {
            this.userSelected.splice(index, 1);
          }
        }
      })
      if(check) {
        data.checked = true;
        this.userSelected.push(data);
        if(this.discount.users) {
          if(this.userSelected.length + this.discount.users.length == this.userData.length) {
            this.checkedAllUser = true;
          }
        }else {
          if(this.userSelected.length == this.userData.length) {
            this.checkedAllUser = true;
          }
        }
      }
    }
  }
  onCheckboxChangeAllUser() {
    this.checkedAllUser = !this.checkedAllUser;
    if(this.checkedAllUser) {
      this.userSelected = [];
      this.userData.forEach((x: any) => {
        let check: boolean = true;
        if(this.discount.users) {
          this.discount.users.forEach((y: any) => {
            if(x.id == y.id) check = false;
          })
        }
        if(check) this.userSelected.push(x);
      })
      this.userSelected.forEach((x: any) => {
        x.checked = true;
      })
    }else {
      this.userSelected.forEach((x: any) => {
        x.checked = false;
      })
      this.userSelected = [];
    }
  }
  sendToUser() {
    if(this.userSelected.length > 0) {
      try{
        const userId = this.userSelected.map((x: any) => x.id);
        const data: any = {
          discountId: this.discount$.id,
          isUse: false,
          userIds: userId
        }
        const sub = this.discountService.addUser(data).subscribe((res: any) => {
          if(res.status) {
            this.cancelClicked.emit();
            this._notifi.showSuccess('Gắn mã giảm giá ' +this.discount$.code+ ' cho người dùng thành công', notifi.SUCCESS);
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
    }else{
      this._notifi.showInfo('Chọn tối thiểu 1 bản ghi', notifi.INFO);
    }
  }
  //#endregion

  //#region ACTION PRODUCT
  checkProductExist(data: any) {
    this.discount.products.filter((m: any) => {
      if (m.id == data.id) {
        data.checked = true;
        data.disabled = true;
      }
    })
  }
  onCheckboxChangeProduct(data: any) {
    if(this.checkedAllProduct) {
      this.checkedAllProduct = false;
      this.productSelected.forEach((x: any) => {
        if(x.id == data.id) {
          data.checked = false;
          const index = this.productSelected.indexOf(x);
          if (index >= 0) {
            this.productSelected.splice(index, 1);
          }
        }
      })
    }else {
      let check: boolean = true;
      this.productSelected.forEach((x: any) => {
        if(x.id == data.id) {
          check = false;
          data.checked = false;
          const index = this.productSelected.indexOf(x);
          if (index >= 0) {
            this.productSelected.splice(index, 1);
          }
        }
      })
      if(check) {
        data.checked = true;
        this.productSelected.push(data);
        if(this.discount.products) {
          if(this.productSelected.length + this.discount.products.length == this.productData.length) {
            this.checkedAllProduct = true;
          }
        }else {
          if(this.productSelected.length == this.productData.length) {
            this.checkedAllProduct = true;
          }
        }
      }
    }
  }
  onCheckboxChangeAllProduct() {
    this.checkedAllProduct = !this.checkedAllProduct;
    if(this.checkedAllProduct) {
      this.productSelected = [];
      this.productData.forEach((x: any) => {
        let check: boolean = true;
        if(this.discount.products) {
          this.discount.products.forEach((y: any) => {
            if(x.id == y.id) check = false;
          })
        }
        if(check) this.productSelected.push(x);
      })
      this.productSelected.forEach((x: any) => {
        x.checked = true;
      })
    }else {
      this.productSelected.forEach((x: any) => {
        x.checked = false;
      })
      this.productSelected = [];
    }
  }
  send() {
    if(!this.isCount) this.countItem();
    if(this.productSelected.length > 0 || this.totalProduct > 0) {
      if(!this.quantityDiscount || this.quantityDiscount == 0) {
        this._notifi.showInfo('Vui lòng nhập số lượt sử dụng của mã giảm giá ' +this.discount$.code+ ', tối thiểu là 1', notifi.INFO);
      }else {
        if(this.productSelected.length > 0) {
          this.sendToProduct();
        }
        if(this.totalProduct > 0) {
          this.sendToProductItem();
        }
      }
    }else{
      this._notifi.showInfo('Chọn tối thiểu 1 bản ghi', notifi.INFO);
    }
  }
  sendToProduct() {
    try{
      const productId = this.productSelected.map((x: any) => x.id);
      const data: any = {
        discountId: this.discount$.id,
        quantity: this.quantityDiscount ?? 1,
        productIds: productId
      }
      const sub = this.discountService.addProduct(data).subscribe((res: any) => {
        if(res.status) {
          this.cancelClicked.emit();
          this._notifi.showSuccess('Gắn mã giảm giá ' +this.discount$.code+ ' cho sản phẩm thành công', notifi.SUCCESS);
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
  sendToProductItem() {
    try {
      const data: any = {
        discountId: this.discount$.id,
        quantity: this.quantityDiscount ?? 1,
        productItemIds: this.productItem$
      }
      const sub = this.discountService.addProductItem(data).subscribe((res: any) => {
        if(res.status) {
          this.cancelClicked.emit();
          this._notifi.showSuccess('Gắn mã giảm giá ' +this.discount$.code+ ' cho biến thể sản phẩm thành công', notifi.SUCCESS);
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
  totalProduct = 0;
  productItem$: any = [];
  isCount: boolean = false;
  countItem() {
    this.isCount = true;
    this.variantSelected.forEach((x: any) => {
      x.valueDetails.forEach((y: any) => {
        if(y.checked) {
          this.totalProduct += 1;
          this.productItem$.push(y.productItemId);
        }
      })
    })
  }
  //#endregion

  //#region EVENT
  reloadUser(): void {
    this.pageUser = 1;
    this.pageSizeUser = 100;
    this.keywordUser = "";
    this.roleId = "";
    this.userRank = "";
    this.initUser();
  }
  reloadProduct(): void {
    this.pageProduct = 1;
    this.pageSizeProduct = 100;
    this.keywordProduct = "";
    this.brandId = "";
    this.skinTypeId = "";
    this.categoryId = ""
    this.initProduct();
  }
  cancel() {
    this.cancelClicked.emit();
  }
  //#endregion

  //#region FILTER
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

  //#region DIALOG
  openVariant(product: any) {
    this.checkSelected(product.id);
    let data = {
      product: product,
      productItemslected: this.discount.productItems,
      variantSelected: this.variantSelected
    }
    const modalRef = this._modalService.openDialogTemplate(DiscountVariantComponent, {
      size: 'xl',
    })
    modalRef.componentInstance.dataField = data;
    try{
      modalRef.componentInstance.saveClicked.subscribe((result: any) => {
        // console.log(result);
        // result.forEach((x: any) => {
        //   x.valueDetails((y: any) => {
        //     if(y.checked) this.totalProduct += 1;
        //   })
        // })
        modalRef.close();
      }
      );
      modalRef.componentInstance.cancelClicked.subscribe(() =>{
        modalRef.close();
      })
    }catch(e) {
      this._notifi.showError(e, notifi.FAIL);
    }
  }
  //#endregion
  getById(productId: any) {
    try{
      const sub = this.productService.getById(productId).subscribe((res: any) => {
        if(res.status) {
          const data = res.data;
          if(data.valueDetails.length == 1 && (!data.valueDetails.vale || data.valueDetails.vale == '')) {
          }else {
            data.valueDetails.filter((item: any) => {
              item.checked = false;
            })
            this.variantSelected.push(data);
          }
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(e) {
      this._notifi.showError(e, notifi.FAIL);
    }
  }

  checkSelected(productId: any) {
    if(this.variantSelected) {
      let check = true;
      this.variantSelected.forEach((item: any) => {
        if(item.id == productId) {
          check = false;
        }
      })
      if(check) {
        this.getById(productId);
      }
    }else this.getById(productId);
  }
}
