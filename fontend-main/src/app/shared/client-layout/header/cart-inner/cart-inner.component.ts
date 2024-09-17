import { ChangeDetectorRef, Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { CartService } from 'src/app/core/services/cart.service';
import { NotificationService } from 'src/app/core/services/notification.service';

@Component({
  selector: 'app-cart-inner',
  templateUrl: './cart-inner.component.html',
  styleUrls: ['./cart-inner.component.scss']
})
export class CartInnerComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  cart$: any = [];
  user: any;

  constructor(private _notifi: NotificationService,
    private fnConstants: constants,
    private route: ActivatedRoute,
    private router: Router,
    public cartService: CartService,
    private cdr: ChangeDetectorRef) {}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
      const cartUser = this.fnConstants.getFromLocalStorage("cartUser");
      if(!cartUser) {
        this.initCart();
      }else {
        this.cart$ = cartUser.data;
        this.cartService.cartTotal = cartUser.totalItem;
      }
    }
    // this.route.paramMap.subscribe(params => {
    // });
    // this.reloadPage();
  }

  initCart() {
    try{
      const sub = this.cartService.search(this.user.id).subscribe((res: any) => {
        if(res.status) {
          this.cart$ = res.data;
          this.cartService.cartTotal = res.totalItem;
          localStorage.setItem("cartUser", JSON.stringify(res));
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

  productItem(product: any) {
    this.router.navigate([routerNav.NAV_PRODUCT_ITEM_CLIENT, product.id]).then(() => {
      this.ngOnInit();
    });
  }
  routerCart() {
    this.router.navigate([routerNav.NAV_CART]);
  }
}
