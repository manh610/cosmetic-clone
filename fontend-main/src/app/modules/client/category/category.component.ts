import { Component, OnInit, OnDestroy, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ModalService } from 'src/app/common/modal/modal.service';
import { notifi, routerNav } from 'src/app/core/models/constants';
import { CategoryService } from 'src/app/core/services/category.service';
import { FormControl, FormGroup } from '@angular/forms';
import { TreeviewComponent } from 'src/app/common/treeview/treeview.component';

@Component({
  selector: 'app-category',
  templateUrl: './category.component.html',
  styleUrls: ['./category.component.scss']
})
export class CategoryComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];

  [x: string]: any;
  treeData:any = [];
  @ViewChild(TreeviewComponent) tree!: any;

  responsiveOptions;
  categoryTree$: any;
  category: any;


  recommendProducts: any;
  formGroup!: FormGroup;
  nodes!: any[];
  selectedNodes: any;

  constructor(
    private _notifi: NotificationService,
    private router: Router,
    private categoryService: CategoryService,
  ) {
    this.responsiveOptions = [
      {
        breakpoint: '1600px',
        numVisible: 6,
        numScroll: 6
      },
      {
        breakpoint: '1200px',
        numVisible: 6,
        numScroll: 6
      },
      {
        breakpoint: '992px',
        numVisible: 5,
        numScroll: 5
      },
      {
        breakpoint: '768px',
        numVisible: 4,
        numScroll: 4
      },
      {
        breakpoint: '576px',
        numVisible: 3,
        numScroll: 3
      },
    ];
    this.initCategoryTree();
  }

  ngOnDestroy() {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
	ngOnInit() {
    this.recommendProducts = [
      {
        "id": "1000",
        "code": "f230fh0g3",
        "name": "Mũ lưỡi trai HEART chất liệu vải kaki, form mềm thời trang, nón lưỡi trai nam nữ LOULI SAIGON M09",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/sg-11134201-7qvdc-lhhk9cfxuepgfc",
        "price": 65000,
        "category": "Accessories",
        "quantity": 24,
        "inventoryStatus": "INSTOCK",
        "rating": 5
      },
      {
        "id": "1001",
        "code": "nvklal433",
        "name": "lồng bàn gấp gọn 5 tầng nhựa cao cấp eccox, lồng bàn cửa kéo mặt kính",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/vn-11134201-7r98o-lktxrgem0myj6a",
        "price": 72000,
        "category": "Accessories",
        "quantity": 61,
        "inventoryStatus": "INSTOCK",
        "rating": 4
      },
      {
        "id": "1002",
        "code": "zz21cz3c1",
        "name": "Thắt lưng nam Dabaraac da cao cấp khóa kim loại tự động không gỉ - Cam kết 1 đổi 1 bảo hành 12 tháng TL58",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/vn-11134201-7r98o-lktxrgds1uww44",
        "price": 79000,
        "category": "Fitness",
        "quantity": 2,
        "inventoryStatus": "LOWSTOCK",
        "rating": 3
      },
      {
        "id": "1003",
        "code": "244wgerg2",
        "name": "Móc Khóa Hình Thỏ Hoạt Hình Đáng Yêu Dành Cho Cặp Đôi",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/vn-11134201-7r98o-lktxrgl9qw8w70",
        "price": 29000,
        "category": "Clothing",
        "quantity": 25,
        "inventoryStatus": "INSTOCK",
        "rating": 5
      },
      {
        "id": "1004",
        "code": "h456wer53",
        "name": "Bracelet",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/31234a27876fb89cd522d7e3db1ba5ca_tn&quot",
        "price": 15000,
        "category": "Accessories",
        "quantity": 73,
        "inventoryStatus": "INSTOCK",
        "rating": 4
      },
      {
        "id": "1005",
        "code": "av2231fwg",
        "name": "Phụ kiện trang sức nữ sức nữ",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/vn-11134201-7r98o-lktxrg0qkxtnbe",
        "price": 120000,
        "category": "Accessories",
        "quantity": 0,
        "inventoryStatus": "OUTOFSTOCK",
        "rating": 4
      },
      {
        "id": "1006",
        "code": "bib36pfvm",
        "name": "Chakra Bracelet",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/vn-50009109-32236a1f5d047010b8f0ee3aa5ac5eda",
        "price": 32000,
        "category": "Accessories",
        "quantity": 5,
        "inventoryStatus": "LOWSTOCK",
        "rating": 3
      },
      {
        "id": "1007",
        "code": "mbvjkgip5",
        "name": "Galaxy Earrings",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/vn-11134207-7r98o-lktqbmgc1j2801",
        "price": 34000,
        "category": "Accessories",
        "quantity": 23,
        "inventoryStatus": "INSTOCK",
        "rating": 5
      },
      {
        "id": "1008",
        "code": "vbb124btr",
        "name": "Game Controller",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/vn-11134207-7r98o-lktqbmgbui8027",
        "price": 99000,
        "category": "Electronics",
        "quantity": 2,
        "inventoryStatus": "LOWSTOCK",
        "rating": 4
      },
      {
        "id": "1009",
        "code": "cm230f032",
        "name": "Gaming Set",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/sg-11134201-7qvg3-lfhnkprmhaj2ed",
        "price": 290009,
        "category": "Electronics",
        "quantity": 63,
        "inventoryStatus": "INSTOCK",
        "rating": 3
      }
      ,
      {
        "id": "1009",
        "code": "cm230f032",
        "name": "Gaming Set",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/sg-11134201-7qvfy-lfhnkq93rq2j68",
        "price": 290009,
        "category": "Electronics",
        "quantity": 63,
        "inventoryStatus": "INSTOCK",
        "rating": 3
      },
      {
        "id": "1009",
        "code": "cm230f032",
        "name": "Gaming Set",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/sg-11134201-7qvex-lfhnkqv0vn4520",
        "price": 290009,
        "category": "Electronics",
        "quantity": 63,
        "inventoryStatus": "INSTOCK",
        "rating": 3
      },
      {
        "id": "1009",
        "code": "cm230f032",
        "name": "Gaming Set",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/sg-11134201-7qvf9-lfhnkrgnzyp3a5",
        "price": 290009,
        "category": "Electronics",
        "quantity": 63,
        "inventoryStatus": "INSTOCK",
        "rating": 3
      },
      {
        "id": "1009",
        "code": "cm230f032",
        "name": "Gaming Set",
        "description": "Product Description",
        "image": "https://down-vn.img.susercontent.com/file/sg-11134201-7qven-lfhnks214p84f6",
        "price": 290009,
        "category": "Electronics",
        "quantity": 63,
        "inventoryStatus": "INSTOCK",
        "rating": 3
      }
    ]
  }

  //#region INIT
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
  //#endregion

  //#region EVENT
  nodeClick(data: any) {
  }
  expandClick(isExpan:any,data: any) {
  }
  //#endregion
}
