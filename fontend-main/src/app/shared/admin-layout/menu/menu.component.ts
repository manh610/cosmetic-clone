import { Component, Input} from '@angular/core';
import { MenuItem } from 'src/app/core/models/menu';
import { MenuService } from 'src/app/core/services/menu.service';

@Component({
  selector: 'app-menu',
  templateUrl: './menu.component.html',
  styleUrls: ['./menu.component.scss']
})
export class MenuComponent{
  menu: Array<any> = [];
  messageOpened: any = "Menu parrent";
  @Input() dataOpened = "open ";

  constructor(private menuService: MenuService) {
  }
  ngOnInit() {
    this.getMenuNav();
  }
  getMenuNav(){
    let menuItem: MenuItem[] = [];
    // menuItem = [{
      // id: 1,
      // name: 'Quản lý người dùng',
      // route: '',
      // icon: 'fas fa-solid fa-user-gear',
      // path: '/admin/user',
      // disabled: false,
      // children: []
    // },
    // {
    //   id: 2,
    //   name: 'Quản lý loại da',
    //   route: '',
    //   icon: 'fas fa-solid fa-mask',
    //   path: '/admin/skin-type',
    //   disabled: false,
    //   children: []
    // },
    // {
    //   id: 3,
    //   name: 'Quản lý thương hiệu',
    //   route: '',
    //   icon: 'fas fa-regular fa-building',
    //   path: '/admin/brand',
    //   disabled: false,
    //   children: []
    // },
    // {
    //   id: 4,
    //   name: 'Quản lý sản phẩm',
    //   route: '',
    //   icon: 'fas fa-solid fa-shop',
    //   path: '/admin/product',
    //   disabled: false,
    //   children: [
        // {
        //   id: 41,
        //   name: 'Quản lý danh mục',
        //   route: '',
        //   icon: '',
        //   path: '/admin/category',
        //   disabled: false,
        //   children: []
        // },
        // {
        //   id: 42,
        //   name: 'Quản lý thuộc tính',
        //   route: '',
        //   icon: '',
        //   path: '/admin/product/attribute',
        //   disabled: false,
        //   children: []
        // },
        // {
        //   id: 43,
        //   name: 'Quản lý sản phẩm',
        //   route: '',
        //   icon: '',
        //   path: '/admin/product',
        //   disabled: false,
        //   children: []
        // }
    //   ]
    // }]
    menuItem = [
      {
        id: 2001,
        name: 'Trang chủ',
        route: '',
        icon: 'fas fa-solid fa-house',
        path: '/admin',
        disabled: false,
        children: []
      },
      {
        id: 1,
        name: 'Quản lý người dùng',
        route: '',
        icon: 'fas fa-solid fa-user-gear',
        path: '/admin/user',
        disabled: false,
        children: []
      },
      // {
      //   id: 2,
      //   name: 'Quản lý cung cấp',
      //   route: '',
      //   icon: 'fas fa-solid fa-user-gear',
      //   path: '',
      //   disabled: false,
      //   children: [
      //     {
      //       id: 21,
      //       name: 'Nhà cung cấp',
      //       route: '',
      //       icon: '',
      //       path: '/admin/supplier',
      //       disabled: false,
      //       children: []
      //     },
      //     {
      //       id: 21,
      //       name: 'Nhập sản phẩm',
      //       route: '',
      //       icon: '',
      //       path: '',
      //       disabled: false,
      //       children: []
      //     },
      //   ]
      // },
      {
        id: 3,
        name: 'Quản lý sản phẩm',
        route: '',
        icon: 'fa-brands fa-galactic-senate',
        path: '#',
        disabled: false,
        children: [
          {
            id: 31,
            name: 'Thương hiệu',
            route: '',
            icon: '',
            path: '/admin/brand',
            disabled: false,
            children: []
          },
          {
            id: 32,
            name: 'Danh mục',
            route: '',
            icon: '',
            path: '/admin/category',
            disabled: false,
            children: []
          },
          {
            id: 33,
            name: 'Sản phẩm',
            route: '',
            icon: '',
            path: '/admin/product',
            disabled: false,
            children: []
          },
          {
            id: 34,
            name: 'Loại da',
            route: '',
            icon: '',
            path: '/admin/skin-type',
            disabled: false,
            children: []
          },
        ]
      },
      // {
      //   id: 4,
      //   name: 'Quản lý biến thể',
      //   route: '',
      //   icon: 'fa-brands fa-galactic-senate',
      //   path: '#',
      //   disabled: false,
      //   children: [
      //     {
      //       id: 42,
      //       name: 'Quản lý thuộc tính',
      //       route: '',
      //       icon: '',
      //       path: '/admin/product/attribute',
      //       disabled: false,
      //       children: []
      //     },
      //   ]
      // },
      {
        id: 4,
        name: 'Quản lý khuyến mại',
        route: '',
        icon: 'fa-solid fa-ticket',
        path: '#',
        disabled: false,
        children: [
          {
            id: 4,
            name: 'Mã giảm giá',
            route: '',
            icon: '',
            path: '/admin/discount',
            disabled: false,
            children: []
          }
        ]
      },
      {
        id: 4,
        name: 'Quản lý đơn hàng',
        route: '',
        icon: 'fa-solid fa-bookmark',
        path: '/admin/order',
        disabled: false,
        children: []
      },
      {
        id: 5,
        name: 'Báo cáo thống kê',
        route: '',
        icon: 'fa-solid fa-chart-pie',
        path: '#',
        disabled: false,
        children: []
      }
    ]
    this.menu = menuItem
    // this.menu = this.menuService.getMenu();
  //   let menuItem: MenuItem[] = [];
  //   try {
  //     this.menuService.getMenuNavByPermission()
  //       .subscribe((resp: any) => {
  //         menuItem = resp.data;
  //         this.menu = menuItem;
  //         localStorage.setItem("menus", JSON.stringify(this.menu));
  //       });
  //     return menuItem;
  //   }
  //   catch (error) {
  //     console.error(error);
  //     return menuItem;
  //   }
  }
}
