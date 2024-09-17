import { ChangeDetectorRef, Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, Subscription, map, mergeMap, tap } from 'rxjs';
import { TreeviewComponent } from 'src/app/common/treeview/treeview.component';
import { notifi, routerNav } from 'src/app/core/models/constants';
import { BrandService } from 'src/app/core/services/brand.service';
import { CategoryService } from 'src/app/core/services/category.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ProductService } from 'src/app/core/services/product/product.service';
import { SkinTypeService } from 'src/app/core/services/skin-type.service';
import { Location } from '@angular/common';
import { StatisticService } from 'src/app/core/services/statistic.service';
import { HttpClientService } from 'src/app/core/services/httpClient.service';

@Component({
  selector: 'app-search',
  templateUrl: './search.component.html',
  styleUrls: ['./search.component.scss']
})
export class SearchComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];
  treeData:any = [];
  @ViewChild(TreeviewComponent) tree!: any;

  rangeValues: number[] = [20, 80];

  product$: any;
  brand$: any;
  skinType$: any;

  keyword: any = "";
  brandId: any = "";
  skinTypeId: any = "";
  categoryId: any = "";
  pageIndex: any = 1;
  pageSize: any = 10;

  formId: any;
  filter: any;
  fromPrice: any;
  toPrice: any;

  isValid: boolean = true;

  selectedBrand: any = null;
  selectedSkinType: any = null;

  constructor(
    private _route: ActivatedRoute,
    private router: Router,
    private _notifi: NotificationService,
    private productService: ProductService,
    private categoryService: CategoryService,
    private brandService: BrandService,
    private skinTypeService: SkinTypeService,
    private location: Location,
    private httpService: HttpClientService,
    private statisticService: StatisticService
  ) {}

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {

    this.filter = this._route.snapshot.paramMap.get('filter');
    if(this.filter == 'keyword') {
      this.keyword = this._route.snapshot.paramMap.get('keyword');
    }
    if(this.filter == 'brand') {
      this.brandId = this._route.snapshot.paramMap.get('keyword');
      this.selectedBrand = this.brandId;
    }
    if(this.filter == 'skinType') {
      this.skinTypeId = this._route.snapshot.paramMap.get('keyword');
      this.selectedSkinType = this.skinTypeId;
    }
    if(this.filter == 'category') {
      this.categoryId = this._route.snapshot.paramMap.get('keyword');
    }
    this.initCategoryTree()
    this.initProduct();
    this.initBrand();
    this.initSkinType();
  }

  //#endregion INIT
  initProduct() {
    try{
      const data: any = {
        keyword: this.keyword,
        status: 'STOCK',
        brandId: this.brandId,
        skinTypeId: this.skinTypeId,
        categoryId: this.categoryId,
        min: this.fromPrice ?? '',
        max: this.toPrice ?? '',
        isDate: false,
        pageIndex: this.pageIndex,
        pageSize: this.pageSize
      }
      const sub = this.productService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.product$ = res.data;
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

  initCategoryTree() {
    try{
      const sub = this.categoryService.buildTree().subscribe((res: any) => {
        if(res.status) {
          this.treeData = res.data;
          // this.categoryTree$ = Promise.resolve(res.data)
          // this.categoryTree$.then((item: any) => (this.nodes = item));
          this.tree.renderNodeChanges(this.treeData);
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
  //#endregion

  //#region EVENT
  nodeClick(data: any) {
    this.categoryId = data.id;
    this.initProduct();
  }
  expandClick(isExpan:any,data: any) {
  }
  reloadPage(): void {
    const currentUrl = this.router.url;
    this.router.navigateByUrl('/', { skipLocationChange: true }).then(() => {
      this.router.navigate([currentUrl]);
      window.location.reload();
    });
  }
  productItem(product: any) {
    this.router.navigate([routerNav.NAV_PRODUCT_ITEM_CLIENT, product.id]);
  }
  use() {
    if((this.fromPrice || this.toPrice)) {
      if(this.fromPrice < 0 || this.toPrice < 0) this.isValid = false;
      else if(this.fromPrice && this.toPrice) {
        if(this.fromPrice > this.toPrice) this.isValid = false;
        else {
          this.isValid = true;
          this.initProduct();
        }
      }
      else {
        this.isValid = true;
        this.initProduct();
      }
    }
  }
  onSlideChange() {
  }
  //#endregion

  //#region FILTER
  filterBrand(event: any) {
    this.brandId = event.value;
    this.initProduct();
  }
  filterSkinType(event: any) {
    this.skinTypeId = event.value;
    this.initProduct();
  }
  newest() {
    try{
      const data: any = {
        keyword: this.keyword,
        status: 'STOCK',
        brandId: this.brandId,
        skinTypeId: this.skinTypeId,
        categoryId: this.categoryId,
        min: this.fromPrice ?? '',
        max: this.toPrice ?? '',
        isDate: false,
        pageIndex: this.pageIndex,
        pageSize: this.pageSize
      }
      const sub = this.statisticService.newest(data).subscribe((res: any) => {
        if(res.status) {
          this.product$ = res.data;
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

  searchCategory() {
    this.categoryId = '';
    this.initProduct();
  }
  searchBrand() {
    this.brandId = '';
    this.initProduct();
  }
  searchSkinType() {
    this.skinTypeId = '';
    this.initProduct();
  }
  //#endregion
}
