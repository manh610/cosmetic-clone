import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AdminComponent } from './admin.component';
import { DashboardComponent } from './dashboard/dashboard.component';
import { UserManagementComponent } from './user-management/user-management.component';
import { CategoryManagementComponent } from './category-management/category-management.component';
import { BrandManagementComponent } from './brand-management/brand-management.component';
import { SkinTypeManagementComponent } from './skin-type-management/skin-type-management.component';
import { SupplierManagementComponent } from './supplier-management/supplier-management.component';
import { DiscountManagementComponent } from './discount-management/discount-management.component';
import { RoleGuardService } from 'src/app/core/services/auth/role.service';
import { OrderManagementComponent } from './order-management/order-management.component';

const routes: Routes = [
  {
    path: '',
    component: AdminComponent,
    children: [
      {
        path: '',
        component: DashboardComponent,
        title: 'Dashboard',
      },
      {
        path: 'user',
        loadChildren: () => import("./user-management/user-management.module").then((m) => m.UserManagementModule),
        title: 'Quản lý người dùng',
        
      },
      {
        path: 'category',
        component: CategoryManagementComponent,
        title: 'Quản lý danh mục sản phẩm',
      },
      {
        path: 'brand',
        component: BrandManagementComponent,
        title: 'Quản lý thương hiệu',

      },
      {
        path: 'skin-type',
        component: SkinTypeManagementComponent,
        title: 'Quản lý loại da',
      },
      {
        path: 'product',
        loadChildren: () => import("./product-management/product-management.module").then((m) => m.ProductManagementModule),
        title: 'Quản lý sản phẩm',
      },
      {
        path: 'supplier',
        component: SupplierManagementComponent,
        title: 'Quản lý nhà cung cấp',
      },
      {
        path: 'discount',
        component: DiscountManagementComponent,
        title: 'Quản lý mã giảm giá',
      },
      {
        path: 'order',
        component: OrderManagementComponent,
        title: 'Quản lý đơn hàng',
      },
    ]
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class AdminRoutingModule { }
