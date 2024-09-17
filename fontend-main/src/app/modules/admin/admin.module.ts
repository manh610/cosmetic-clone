import { NgModule } from "@angular/core";
import { AdminComponent } from "./admin.component";
import { CommonModule } from '@angular/common';
import { AdminRoutingModule } from "./admin-routing.module";
import { DashboardComponent } from './dashboard/dashboard.component';
import { HeaderComponent } from "src/app/shared/admin-layout/header/header.component";
import { MenuComponent } from "src/app/shared/admin-layout/menu/menu.component";
import { SideNavComponent } from "src/app/shared/admin-layout/side-nav/side-nav.component";
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatIconModule } from '@angular/material/icon';
import { MatMenuModule } from "@angular/material/menu";
import { TranslateModule } from "@ngx-translate/core";
import { AgGridModule } from "ag-grid-angular";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { MatListModule } from '@angular/material/list';
import { MenuItemComponent } from "src/app/shared/admin-layout/menu/menu-item/menu-item.component";
import { NotificationInnerComponent } from "src/app/shared/admin-layout/header/notification-inner/notification-inner.component";
import { TableModule } from 'primeng/table';
import { ButtonModule } from 'primeng/button';
import { ColorPickerModule } from 'ngx-color-picker';
import { UserManagementModule } from "./user-management/user-management.module";
import { CategoryManagementModule } from "./category-management/category-management.module";
import { MatSelectModule } from "@angular/material/select";
import { NgxMatSelectSearchModule } from 'ngx-mat-select-search';
import { MatFormField, MatFormFieldModule } from "@angular/material/form-field";
import { BrandManagementModule } from "./brand-management/brand-management.module";
import { SkinTypeManagementModule } from "./skin-type-management/skin-type-management.module";
import { ProductManagementModule } from "./product-management/product-management.module";
import { SupplierManagementModule } from "./supplier-management/supplier-management.module";
import { DiscountManagementModule } from "./discount-management/discount-management.module";
import { DashboardModule } from "./dashboard/dashboard.module";
import { OrrderManagementModule } from "./order-management/order-management.module";

@NgModule({
  declarations: [
    AdminComponent,
    HeaderComponent,
    MenuComponent,
    SideNavComponent,
    MenuItemComponent,
    NotificationInnerComponent
  ],
  imports: [
    CommonModule,
    AdminRoutingModule,
    TranslateModule,
    AgGridModule,
    FormsModule,
    ReactiveFormsModule,
    MatSidenavModule,
    MatToolbarModule,
    MatIconModule,
    MatMenuModule,
    MatListModule,
    TableModule,
    ButtonModule,
    MatFormFieldModule,
    MatSelectModule,
    NgxMatSelectSearchModule,
    ColorPickerModule,
    UserManagementModule,
    CategoryManagementModule,
    BrandManagementModule,
    SkinTypeManagementModule,
    ProductManagementModule,
    SupplierManagementModule,
    DiscountManagementModule,
    DashboardModule,
    OrrderManagementModule
  ]
})
export class AdminModule { }
