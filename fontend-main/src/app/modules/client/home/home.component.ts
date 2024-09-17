import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ModalService } from 'src/app/common/modal/modal.service';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { AuthStore } from 'src/app/core/stores/auth.store';
import { CategoryService } from 'src/app/core/services/category.service';
import { ProductService } from 'src/app/core/services/product/product.service';
import { DiscountService } from 'src/app/core/services/discount.service';
import { formatDate } from '@angular/common';
import { BrandService } from 'src/app/core/services/brand.service';
import { SkinTypeService } from 'src/app/core/services/skin-type.service';
import { CartService } from 'src/app/core/services/cart.service';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.scss']
})
export class HomeComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];

  recommendProducts: any;
  responsiveOptions;
  categoryRoot$: any;
  brand$: any;
  skinType$: any;

  page: number = 1;
  pageSize: number = 10;
  totalItem: any;
  product$: any;
  discount$: any;
  constructor(
    private authStore: AuthStore,
    private _notifi: NotificationService,
    private router: Router,
    private _constants: constants,
    private brandService: BrandService,
    private categoryService: CategoryService,
    private productService: ProductService,
    private discountService: DiscountService,
    private skinTypeService: SkinTypeService,
    private cartService: CartService
  ) {
    this.responsiveOptions = [
      {
        breakpoint: '1600px',
        numVisible: 10,
        numScroll: 10
      },
      {
        breakpoint: '1200px',
        numVisible: 8,
        numScroll: 8
      },
      {
        breakpoint: '992px',
        numVisible: 6,
        numScroll: 6
      },
      {
        breakpoint: '768px',
        numVisible: 5,
        numScroll: 5
      },
      {
        breakpoint: '576px',
        numVisible: 3,
        numScroll: 3
      },
    ];
  }

  ngOnDestroy() {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this._constants.getFromLocalStorage("currentUser");
    this.initDiscount();
    this.initBrand();
    this.initSkinType();
    this.initCategoryRoot();
    this.initProduct();
  }

  //#region INIT
  initDiscount() {
    try{
      const data: any = {
        keyword: '',
        discountType: 'PROMOTION',
        pageIndex: 1,
        pageSize: 100
      }
      const sub = this.discountService.search(data).subscribe((res: any) => {
        if(res.status) {
          const currentDate = formatDate(new Date().toLocaleString(), 'yyyy-MM-ddTHH:mm', 'en-US');
          this.discount$ = res.data.filter((x: any) => (x.endDate >= currentDate && x.show == true))
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
  initCategoryRoot() {
    try{
      const sub = this.categoryService.root().subscribe((res: any) => {
        if(res.status) {
          this.categoryRoot$ = res.data;
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
  initCart() {
    // try{
    //   const sub = this.cartService.search('1').subscribe((res: any) => {
    //     if(res.status) {
    //       this.cartService.cartTotal = res.totalItem;
    //     }
    //   }, (error: any) => {
    //     for (let e of error.error.errors) {
    //       this._notifi.showInfo(e, notifi.FAIL);
    //     }
    //   })
    // }catch(ex) {
    //   this._notifi.showError(ex, notifi.FAIL);
    // }
  }
  //#endregion

  //#region PRODUCT
  initProduct() {
    try{
      let data: any = {
        keyword: '',
        status: 'STOCK',
        brandId: '',
        categoryId: '',
        skinTypeId: '',
        isDate: false,
        pageIndex: this.page,
        pageSize: this.pageSize
      }
      const sub = this.productService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.totalItem = res.totalItem;
          if (this.page > 1 && res.data){
              res.data.forEach((item: any) =>{
                this.product$.push(item);
            })
          }
          else{
            this.product$ = res.data;
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

  //#region ACTION
  showMore() {
    this.page += 1;
    this.initProduct();
  }
  //#endregion

  //#region EVENT
  productItem(product: any) {
    this.router.navigate([routerNav.NAV_PRODUCT_ITEM_CLIENT, product.id]);
  }
  categoryItem(category: any) {
    this.router.navigate([routerNav.NAV_CATEGORYSEARCH, category.code, category.id]);
  }
  routerSearch(keyword: any, filter: any) {
    this.router.navigate([routerNav.NAV_SEARCH, keyword, filter]);
  }
  //#endregion
}
