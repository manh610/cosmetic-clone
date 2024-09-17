import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { CartService } from 'src/app/core/services/cart.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { OrderService } from 'src/app/core/services/order.service';

@Component({
  selector: 'app-cart',
  templateUrl: './cart.component.html',
  styleUrls: ['./cart.component.scss']
})
export class CartComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  cart$: any = [];
  user: any;
  cartStock: any = [];

  value: any;
  checked: any;
  productSelected: any = [];
  checkedAll: boolean = false;

  toltalPrice: any = 0;

  constructor(private _notifi: NotificationService,
    private fnConstants: constants,
    private router: Router,
    private orderService: OrderService,
    public cartService: CartService) {}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
    }
    this.initCart();
  }

  initCart() {
    try{
      const sub = this.cartService.search(this.user.id).subscribe((res: any) => {
        if(res.status) {
          this.cart$ = res.data;
          this.cartStock = this.cart$.filter((x: any) => x.valueStatus == 'STOCK');
          localStorage.setItem("cartUser", JSON.stringify(res));
          this.cart$.forEach((x: any) => {
            x.checked = false;
          })
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

  //#region CRUD
  updateCart(quantity: any, data: any) {
    try{
      const sub = this.cartService.update(data.cartId, quantity).subscribe((res: any) => {
        if(res.status) {
          this._notifi.showSuccess('Cập nhật giỏ hàng thành công', notifi.SUCCESS);
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
  removeCart(data: any) {
    try {
      const sub = this.cartService.remove(data.cartId).subscribe((res: any) => {
        this.cartService.cartTotal -= 1;
        this.initCart();
        this._notifi.showSuccess('Xoá sản phẩm từ giỏ hàng thành công', notifi.SUCCESS);
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
  deleteMulti(data: any) {
    try{
      const sub = this.cartService.deleteMulti(data).subscribe((res: any) => {
        this.cartService.cartTotal -= this.productSelected.length;
        this.initCart();
        this._notifi.showSuccess('Xoá sản phẩm từ giỏ hàng thành công', notifi.SUCCESS);
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
  //#endregion

  //#region ACTION
  productItem(product: any) {
    if(product.valueStatus == 'STOCK') {
      this.router.navigate([routerNav.NAV_PRODUCT_ITEM_CLIENT, product.id]);
    }
  }
  onCheckboxChange(data: any) {
    if(this.checkedAll) {
      this.checkedAll = false;
      this.productSelected.forEach((x: any) => {
        if(x.productItemId == data.productItemId) {
          data.checked = false;
          const index = this.productSelected.indexOf(x);
          if (index >= 0) {
            this.productSelected.splice(index, 1);
          }
        }
      })
      this.toltalPrice = this.productSelected.reduce((sum: any, item: any) => sum + (item.sellPrice*item.quantity), 0);
    }else {
      let check: boolean = true;
      this.productSelected.forEach((x: any) => {
        if(x.productItemId == data.productItemId) {
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
        if(this.productSelected.length == this.cartStock.length) {
          this.checkedAll = true;
        }
      }
      this.toltalPrice = this.productSelected.reduce((sum: any, item: any) => sum + (item.sellPrice*item.quantity), 0);
    }
  }
  onCheckboxChangeAll() {
    this.checkedAll = !this.checkedAll;
    if(this.checkedAll) {
      this.productSelected = [];
      this.productSelected = this.cartStock;
      this.productSelected.forEach((x: any) => {
        x.checked = true;
      })
      this.toltalPrice = this.productSelected.reduce((sum: any, item: any) => sum + (item.sellPrice*item.quantity), 0);
    }else {
      this.productSelected.forEach((x: any) => {
        x.checked = false;
      })
      this.productSelected = [];
      this.toltalPrice = 0;
    }
  }
  removeSoldOut() {
    const soldout = this.cart$.filter((x: any) => x.valueStatus != 'STOCK');
    if(soldout.length > 0) {
      const data = soldout.map((x: any) => x.cartId);
      this.deleteMulti(data);
    }else {
      this._notifi.showInfo('Không có sản phẩm hết hàng', notifi.INFO);
    }
  }
  deleteMultiCart() {
    if(this.productSelected.length > 0) {
      const data = this.productSelected.map((x: any) => x.cartId);
      this.deleteMulti(data);
    }else {
      this._notifi.showInfo('Vui lòng chọn tối thiểu 1 sản phẩm', notifi.INFO);
    }
  }
  buy() {
    if(this.productSelected.length == 0) {
      this._notifi.showInfo('Vui lòng chọn tối thiểu 1 sản phẩm để đặt hàng', notifi.INFO);
      return;
    }
    this.productSelected.forEach((x: any) => {
      const data = {
        id: x.valueDetailId,
        image: x.image,
        importPrice: x.importtPrice,
        importQuantity: x.importQuantity,
        productItemDiscounts: [],
        productItemId: x.productItemId,
        sellPrice: x.sellPrice,
        sellQuantity: x.sellQuantity,
        status: x.valueStatus,
        value: x.value
      }
      x.valueDetails = data;
    })
    this.orderService.orderSelected = this.productSelected;
    this.router.navigate([routerNav.NAV_ORDER]);
  }
  //#endregion

  //#region EVENT
  onQuantityChange(event: any, data: any) {
    if(event > data.sellQuantity) return;
    if(event) {
      this.updateCart(event, data);
    }
  }
  //#endregion
}
