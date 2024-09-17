import { formatDate } from '@angular/common';
import { ChangeDetectorRef, Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, NavigationExtras, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { StorageService } from 'src/app/core/services/auth/storage.service';
import { CartService } from 'src/app/core/services/cart.service';
import { FavoriteService } from 'src/app/core/services/favorite.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { OrderService } from 'src/app/core/services/order.service';
import { ProductService } from 'src/app/core/services/product/product.service';
import { ReviewService } from 'src/app/core/services/review.service';
import { UserService } from 'src/app/core/services/user.service';

@Component({
  selector: 'app-product',
  templateUrl: './product.component.html',
  styleUrls: ['./product.component.scss']
})
export class ProductComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];
  value: any = 0;
  value2: any = 1;
  star: any = 0;
  review$: any = [];

  productId: any;
  product$: any;
  variant$: any;
  user$: any;
  favorite$: any;
  isPhoto: boolean = true;
  isVariant: boolean = true;
  photo: any;

  productionDate: any;
  expirationDate: any;
  isFavorite: boolean = false;
  constructor(
    public _route: ActivatedRoute,
    private router: Router,
    private _notifi: NotificationService,
    private fnConstants: constants,
    private storageService: StorageService,
    private userService: UserService,
    private productService: ProductService,
    private cartService: CartService,
    private favoriteService: FavoriteService,
    private orderService: OrderService,
    private cdr: ChangeDetectorRef,
    private reviewService: ReviewService
  ){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.productId = this._route.snapshot.paramMap.get('id');
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user$ = currentUser;
      const favoriteUser = this.fnConstants.getFromLocalStorage("favoriteUser");
      if(favoriteUser) {
        this.favorite$ = favoriteUser.data;
        this.favorite$.forEach((x: any) => {
          if(x.id == this.productId) this.isFavorite = true;
        })
      } else {
        this.initFavorite();
      }
    }
    this.initProductItem();
    this.initReview();
  }

  //#region INIT
  initProductItem() {
    try{
      const sub = this.productService.getById(this.productId).subscribe((res: any) => {
        if(res.status){
          this.product$ = res.data;
          this.value = this.product$.totalStar;
          if(this.product$.valueDetails.length == 1 && (this.product$.valueDetails[0].value == "" || !this.product$.valueDetails[0].value)) {
            this.variant$ = this.product$.valueDetails[0];
            this.isVariant = false;
          }
          this.productionDate = this.formattedDate(this.product$.productionDate);
          this.expirationDate = this.formattedDate(this.product$.expirationDate);
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

  initReview() {
    try{
      const data = {
        star: this.star == 0 ? '' : this.star
      }
      const sub = this.reviewService.search(this.productId, data).subscribe((res: any) => {
        if(res.status) {
          this.review$ = res.data;
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
  addToCart() {
    if(this.variant$) {
      if(this.user$) {
        if(this.value2 > this.product$.totalQuantity) {
          this._notifi.showInfo('Vượt quá số lượng trong kho', notifi.INFO);
          return;
        }
        try{
          const data: any = {
            productItemId: this.variant$.productItemId,
            userId: this.user$.id,
            quantity: this.value2
          }
          const sub = this.cartService.addProduct(data).subscribe((res: any) => {
            if(res.status) {
              this.initCart();
              this._notifi.showSuccess('Thêm sản phẩm vào giỏ hàng thành công', notifi.SUCCESS);
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
      }else {
        this.router.navigate([routerNav.NAV_LOGIN]);
      }
    }else {
      this._notifi.showInfo("Vui lòng chọn 1 biến thể sản phẩm", notifi.INFO);
    }
  }

  initCart() {
    try{
      const sub = this.cartService.search(this.user$.id).subscribe((res: any) => {
        if(res.status) {
          this.cartService.cartTotal = res.totalItem;
          localStorage.setItem("cartUser", JSON.stringify(res));
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e, notifi.FAIL);
        }
      })
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }

  addToFavorite() {
    if(this.user$) {
      try{
        const data: any = {
          productId: this.productId,
          userId: this.user$.id
        }
        const sub = this.favoriteService.addProduct(data).subscribe((res: any) => {
          if(res.status) {
            this.isFavorite = true;
            this.initFavorite();
            this.initProductItem();
            this._notifi.showSuccess('Thêm sản phẩm vào danh sách yêu thích thành công', notifi.SUCCESS);
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
    }else {
      this.router.navigate([routerNav.NAV_LOGIN]);
    }
  }

  removeFromFavorite() {
    if(this.user$) {
      const sub = this.favoriteService.removeProduct(this.user$.id, this.productId).subscribe((res: any) => {
        this.isFavorite = false;
        this.initFavorite();
        this.initProductItem();
        this._notifi.showSuccess('Xóa sản phẩm từ danh sách yêu thích thành công', notifi.SUCCESS);
      }, (error: any) => {
          for (let e of error.error.errors) {
            this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
          }
      })
      this.unsubscribe.push(sub);
    }else {
      this.router.navigate([routerNav.NAV_LOGIN]);
    }
  }

  initFavorite() {
    try{
      const sub = this.favoriteService.search(this.user$.id).subscribe((res: any) => {
        if(res.status) {
          this.favoriteService.favoriteTotal = res.totalItem;
          localStorage.setItem("favoriteUser", JSON.stringify(res));
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e, notifi.FAIL);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }

  buy() {
    if(this.variant$) {
      if(this.user$) {
        this.product$.valueDetails = this.variant$;
        this.product$.quantity = this.value2;
        this.orderService.orderSelected = this.product$;
        this.router.navigate([routerNav.NAV_ORDER]);
      }else {
        this.router.navigate([routerNav.NAV_LOGIN]);
      }
    }else {
      this._notifi.showInfo("Vui lòng chọn 1 biến thể sản phẩm", notifi.INFO);
    }
  }
  //#endregion

  //#region EVENT
  formattedDate(date: any): string {
    const parsedDate = new Date(date);
    return formatDate(parsedDate, 'dd/MM/yyyy', 'en-US');
  }
  slideImg(data: any) {
    this.isPhoto = false;
    this.photo = data.data;
  }
  variant(data: any) {
    this.isPhoto = true;
    this.variant$ = data;
  }

  filterStar(star: any) {
    this.star = star;
    this.initReview();
  }
  reloadPage(): void {
    this.router.routeReuseStrategy.shouldReuseRoute = () => false;
    const currentUrl = this.router.url;
    this.router.navigateByUrl('/', { skipLocationChange: true }).then(() => {
      this.cdr.detectChanges();
      this.router.navigate([currentUrl]);
    });
    // const currentUrl = this.router.url;
    // this.router.navigateByUrl('/', { skipLocationChange: true }).then(() => {
    //   this.cdr.detectChanges();
    //   this.router.navigate([currentUrl]);
    // });
  }
  //#endregion
}
