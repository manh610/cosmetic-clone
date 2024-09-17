import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { HomeComponent } from './home/home.component';
import { ClientComponent } from './client.component';
import { CategoryComponent } from './category/category.component';
import { AuthGuardService } from 'src/app/core/services/auth/auth-guard.service';
import { ProductComponent } from './product/product.component';
import { SearchComponent } from './search/search.component';
import { CartComponent } from './cart/cart.component';
import { OrderProductComponent } from './order-product/order-product.component';

const routes: Routes = [
  {
    path: '',
    component: ClientComponent,
    children: [
      {
        path: '',
        component: HomeComponent
      },
      {
        path: 'product/:id',
        component: ProductComponent
      },
      {
        path: 'category/:code/:id',
        component: CategoryComponent,
        title: 'Danh mục sản phẩm'
      },
      {
        path: 'ggcos/profile',
        loadChildren: () => import("./user/user.module").then(m => m.UserModule),
        canActivate: [AuthGuardService],
        title: 'Thông tin cá nhân'
      },
      {
        path: 'search',
        loadChildren: () => import("./search/search.module").then(m => m.SearchModule),
        title: 'Tìm kiếm sản phẩm'
      },
      {
        path: 'cart',
        component: CartComponent,
        canActivate: [AuthGuardService],
        title: 'Giỏ hàng'
      },
      // {
      //   path: 'flash-sales',
      //   component: FlashSalesPagesComponent
      // },
      // {
      //   path: 'top-sales',
      //   component: TopSalesPagesComponent
      // },
      // {
      //   path: 'product/:code/:id',
      //   loadChildren: () => import("./list-product-pages/list-product-pages.module").then(m => m.ListProductPagesModule)
      // },
    ]
  }
];


@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]

})
export class ClientRoutingModule { }
