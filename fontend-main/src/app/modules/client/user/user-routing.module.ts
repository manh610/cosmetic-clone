import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { UserComponent } from './user.component';
import { UserInforComponent } from './user-infor/user-infor.component';
import { DiscountUserComponent } from './discount-user/discount-user.component';
import { AddressComponent } from './address/address.component';
import { FavoriteUserComponent } from './favorite-user/favorite-user.component';
import { OrderUserComponent } from './order-user/order-user.component';

const routes: Routes = [
  {
    path: '',
    component: UserComponent,
    children: [
      {path: 'me',component: UserInforComponent},
      {path: '',component: UserInforComponent},
      {path: 'discount',component: DiscountUserComponent},
      // {path: 'notification',component: NotificationSettingComponent},
      {path: 'address',component: AddressComponent},
      {path: 'favorite',component: FavoriteUserComponent},
      {path: 'order',component: OrderUserComponent},
    ]
  }
];
@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class UserRoutingModule { }
