import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { NotFoundComponent } from './not-found/not-found.component';
import { AuthGuard } from './core/utils/auth.guard';
import { RoleGuardService } from './core/services/auth/role.service';
import { OrderProductComponent } from './modules/client/order-product/order-product.component';

const routes: Routes = [
  {
    path: 'authentication',
    loadChildren: () => import("./modules/admin/auth/auth.module").then(m => m.AuthModule),
    title: 'Xác thực'
  },
  {
    path: 'admin',
    loadChildren: () => import("./modules/admin/admin.module").then(m=>m.AdminModule),
    title: 'Quản trị viên',
    canActivate: [RoleGuardService],data: {expectedRole: "00"}
  },
  {
    path: 'order',
    component: OrderProductComponent,
    title: 'Đặt hàng'
  },
  {
    path: '',
    loadChildren: () => import("./modules/client/client.module").then(m => m.ClientModule),
    title: 'GG cosmetic'
  },
  {
    path: '**',
    component: NotFoundComponent,
    pathMatch: 'full'
  },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
