import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { ModalService } from 'src/app/common/modal/modal.service';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { OrderService } from 'src/app/core/services/order.service';
import { UserService } from 'src/app/core/services/user.service';
import { AddressComponent } from './address/address.component';
import { DeliveryUnitService } from 'src/app/core/services/delivery-unit.service';
import { DiscountComponent } from './discount/discount.component';
import { FormBuilder, FormGroup } from '@angular/forms';
import { CartService } from 'src/app/core/services/cart.service';

@Component({
  selector: 'app-order-product',
  templateUrl: './order-product.component.html',
  styleUrls: ['./order-product.component.scss']
})
export class OrderProductComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  note: any = '';
  user$: any;
  address$: any;
  address: any;
  product$: any = [];
  delivery: any;
  totalPrice: any = 0;

  discountUser: any;
  isPayment: boolean = true;
  constructor(private fb: FormBuilder,
    public _route: ActivatedRoute,
    private router: Router,
    private _notifi: NotificationService,
    private fnConstants: constants,
    private orderService: OrderService,
    private userService: UserService,
    private deliveryService: DeliveryUnitService,
    private modalService: ModalService,
    private cartService: CartService
  ){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user$ = currentUser;
      this.initAddress();
      this.initDelivery();
      if(this.orderService.orderSelected.length == undefined) {
        this.product$.push(this.orderService.orderSelected);
        this.totalPrice = this.product$[0].valueDetails.sellPrice * this.product$[0].quantity;
      }else {
        this.product$ = this.orderService.orderSelected;
        this.product$.forEach((x: any) => {
          this.totalPrice += x.valueDetails.sellPrice * x.quantity;
        })
      };
    }
  }

  //#region INIT
  initAddress() {
    try {
      const sub = this.userService.getAddress(this.user$.id).subscribe((res: any) => {
        if(res.status) {
          this.address$ = res.data;
          this.address = this.address$.filter((x: any) => x.default);
          this.address = this.address[0];
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

  initDelivery() {
    try{
      const data: any = {
        keyword: '',
        deliveryType: ''
      }
      const sub = this.deliveryService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.delivery = res.data[0];
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

  initCart() {
    try{
      const sub = this.cartService.search(this.user$.id).subscribe((res: any) => {
        if(res.status) {
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
  //#endregion

  //#region ACTION
  buy() {
    if(!this.address) {
      this._notifi.showInfo('Vui lòng chọn địa chỉ nhận hàng', notifi.INFO);
      return;
    }
    if(this.product$.length == 0) return;
    try{
      let productItem: any = [];
      this.product$.forEach((x: any) => {
        const req = {
          productItemId: x.valueDetails.productItemId,
          quantity: x.quantity
        }
        productItem.push(req);
      })
      let tmp: any = 0;
      if(this.discountUser) tmp = this.totalPrice - this.totalPrice*(this.discountUser?.value*0.01) + this.delivery?.price;
      else tmp = this.totalPrice + this.delivery?.price;
      const data = {
        userId: this.user$.id,
        note: this.note,
        totalPrice: tmp,
        addressId: this.address.id,
        deliveryUnitId: this.delivery.id,
        deliveryType: 'GTN',
        paymentId: null,
        deliveryDate: '',
        receiptDate: '',
        censor: '',
        discountId: this.discountUser?.id,
        productItem: productItem
      }
      const sub = this.orderService.create(data).subscribe((res: any) => {
        if(res.status) {
          this.deleteMultiProductFromCart();
          this._notifi.showSuccess('Đặt hàng thành công', notifi.SUCCESS);
          if(true) this.router.navigate([routerNav.NAV_ORDER_ME]);
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

  deleteMultiProductFromCart() {
    const cartIds: any = [];
    this.product$.forEach((x: any) => {
      if(x.cartId) cartIds.push(x.cartId);
    })
    if(cartIds.length > 0) {
      try{
        const sub = this.cartService.deleteMulti(cartIds).subscribe((res: any) => {
          this.cartService.cartTotal -= cartIds.length;
          this.initCart();
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
  }

  changeAddress(id: any) {
    let data = {
      id: id,
      data: this.address$
    }
    const modalRef = this.modalService.openDialogTemplate(AddressComponent, {
      size: 'md',
    })
    modalRef.componentInstance.dataDialog = data;
    try{
      modalRef.componentInstance.saveClicked.subscribe((result: any)  => {
        this.address = result[0];
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

  selectDiscount() {
    let discountProduct: any = [];
    this.product$.forEach((x: any) => {
      x.productDiscounts?.forEach((y: any) => {
        discountProduct.push(y);
      })
    })
    let data = {
      discountUser: this.discountUser,
      discountProduct: discountProduct
    }
    const modalRef = this.modalService.openDialogTemplate(DiscountComponent, {
      size: 'md',
    })
    modalRef.componentInstance.dataDialog = data;
    try{
      modalRef.componentInstance.saveClicked.subscribe((result: any)  => {
        this.discountUser = result;
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

  changePayment(number: any) {
    if(number == 1) this.isPayment = true;
    else this.isPayment = false;
  }
  //#endregion
}
