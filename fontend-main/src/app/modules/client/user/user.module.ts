import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { CarouselModule } from 'primeng/carousel';
import { ButtonModule } from 'primeng/button';
import { MatTabsModule } from '@angular/material/tabs';
import { MatMenuModule} from '@angular/material/menu';
import { MatIconModule } from '@angular/material/icon';
import { MdbTabsModule } from 'mdb-angular-ui-kit/tabs';
import { TreeSelectModule } from 'primeng/treeselect';
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { CdkTreeModule } from '@angular/cdk/tree';
import { MatTreeModule } from '@angular/material/tree';
import { CustomCommonModule } from 'src/app/common/common.module';
import { UserComponent } from './user.component';
import { UserRoutingModule } from './user-routing.module';
import { UserInforComponent } from './user-infor/user-infor.component';
import { CalendarModule } from 'primeng/calendar';
import { MatRadioModule } from '@angular/material/radio';
import { UserItemComponent } from './user-infor/user-item/user-item.component';
import { ChangePasswordComponent } from './user-infor/change-password/change-password.component';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { DiscountUserComponent } from './discount-user/discount-user.component';
import { AddressComponent } from './address/address.component';
import { AddressItemComponent } from './address/address-item/address-item.component';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { DropdownModule } from 'primeng/dropdown';
import { FavoriteUserComponent } from './favorite-user/favorite-user.component';
import { OrderUserComponent } from './order-user/order-user.component';
import { OrderItemComponent } from './order-user/order-item/order-item.component';
import { OrderFormComponent } from './order-user/order-form/order-form.component';
import { TimelineModule } from 'primeng/timeline';

@NgModule({
  declarations: [
    UserComponent,
    UserInforComponent,
    UserItemComponent,
    ChangePasswordComponent,
    DiscountUserComponent,
    AddressComponent,
    AddressItemComponent,
    FavoriteUserComponent,
    OrderUserComponent,
    OrderItemComponent,
    OrderFormComponent
  ],
  imports: [
    CommonModule,
    CarouselModule,
    ButtonModule,
    MatTabsModule,
    MatMenuModule,
    MatIconModule,
    MdbTabsModule,
    TreeSelectModule,
    FormsModule,
    ReactiveFormsModule,
    CdkTreeModule,
    MatTreeModule,
    CustomCommonModule,
    CalendarModule,
    MatRadioModule,
    UserRoutingModule,
    MatTabsModule,
    MatDatepickerModule,
    MatNativeDateModule,
    MatCheckboxModule,
    DropdownModule,
    TimelineModule
  ],
  providers: [
    MatDatepickerModule,
    MatNativeDateModule
  ],
})
export class UserModule { }
