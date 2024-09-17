import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { notifi } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ProductService } from 'src/app/core/services/product/product.service';

@Component({
  selector: 'app-discount-variant',
  templateUrl: './discount-variant.component.html',
  styleUrls: ['./discount-variant.component.scss']
})
export class DiscountVariantComponent implements OnInit, OnDestroy {
  @Input() dataField: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();
  public unsubscribe: Subscription[] = [];

  isShow: boolean = true;

  productId: any;
  product$: any;
  value$: any;

  valueData: any;
  valueSelected: any = [];
  checkedAll: boolean = false;
  preValueSelected: any = [];
  productItemsSelected: any = [];

  isExist: boolean = false;

  constructor(
    private _notifi: NotificationService,
    private productService: ProductService
  ){}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sub) => sub.unsubscribe());
  }
  ngOnInit(): void {
    this.productId = this.dataField.product.id;
    this.preValueSelected = this.dataField.variantSelected; //biến thể được chọn để gắn mã
    this.productItemsSelected = this.dataField.productItemslected;
    this.getById();
  }

  getById() {
    try{
      const sub = this.productService.getById(this.productId).subscribe((res: any) => {
        if(res.status) {
          this.product$ = res.data;
          this.value$ = res.data.valueDetails;
          if(this.value$.length == 1 && (!this.value$.vale || this.value$.vale == '')) {
            this.isShow = false;
            this._notifi.showInfo('Sản phẩm ' +this.product$.code+ ' không có biến thể', notifi.INFO);
          }else {
            this.valueData = this.value$.filter((x: any) => x.id);
            if(this.productItemsSelected) {
              this.value$.forEach((item: any) => {
                this.checkExist(item);
              })
            }
            this.preValueSelected.forEach((x: any) => {
              if(x.id == this.productId) {
                this.value$.forEach((item: any) => {
                  this.checkSelected(item, x.valueDetails);
                })
              }
            })
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

  //#region ACTION
  checkExist(data: any) {
    this.productItemsSelected.filter((m: any) => {
      if (m.productItemId == data.productItemId) {
        data.checked = true;
        data.disabled = true;
        this.isExist = true;
      }
    })
  }
  checkSelected(data: any, x: any) {
    x.filter((m: any) => {
      if (m.productItemId == data.productItemId && m.checked) {
        data.checked = true;
        this.valueSelected.push(m);
      }
    })
  }
  onCheckboxChange(data: any) {
    if(this.checkedAll) {
      this.checkedAll = false;
      this.valueSelected.forEach((x: any) => {
        if(x.productItemId == data.productItemId) {
          data.checked = false;
          const index = this.valueSelected.indexOf(x);
          if (index >= 0) {
            this.valueSelected.splice(index, 1);
          }
        }
      })
    }else {
      let check: boolean = true;
      this.valueSelected.forEach((x: any) => {
        if(x.productItemId == data.productItemId) {
          check = false;
          data.checked = false;
          const index = this.valueSelected.indexOf(x);
          if (index >= 0) {
            this.valueSelected.splice(index, 1);
          }
        }
      })
      if(check) {
        data.checked = true;
        this.valueSelected.push(data);
        if(this.isExist) {
          if(this.valueSelected.length + this.productItemsSelected.length === this.valueData.length) {
            this.checkedAll = true;
          }
        }else {
          if(this.valueSelected.length === this.valueData.length) {
            this.checkedAll = true;
          }
        }
      }
    }
  }
  onCheckboxChangeAll() {
    this.checkedAll = !this.checkedAll;
    if(this.checkedAll) {
      this.valueSelected = [];
      this.valueData.forEach((x: any) => {
        let check: boolean = true;
        if(this.productItemsSelected) {
          this.productItemsSelected.forEach((y: any) => {
            if(x.productItemId == y.productItemId) check = false;
          })
        }
        if(check) this.valueSelected.push(x);
      })
      this.valueSelected.forEach((x: any) => {
        x.checked = true;
      })
    }else {
      this.valueSelected.forEach((x: any) => {
        x.checked = false;
      })
      this.valueSelected = [];
    }
  }
  //#endregion

  cancel() {
    this.cancelClicked.emit();
  }
  save() {
    try {
      if (this.valueSelected.length == 0) {
      }
      else {
        this.preValueSelected.forEach((x: any) => {
          if(x.id == this.productId) {
            x.valueDetails.forEach((y: any) => {
              let check = false;
              this.valueSelected.forEach((z: any) => {
                if(y.productItemId == z.productItemId) check = true;
              })
              if(check) y.checked = true;
              else y.checked = false;
            })
          }
        })
      }
      return this.saveClicked.emit(this.preValueSelected);
    } catch (ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
}
