import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatMenuModule } from '@angular/material/menu';
import { AgGridModule } from 'ag-grid-angular';
import { MenuComponent } from './admin-layout/menu/menu.component';
import { SideNavComponent } from './admin-layout/side-nav/side-nav.component';
import { MenuItemComponent } from './admin-layout/menu/menu-item/menu-item.component';
import { NotificationInnerComponent } from './admin-layout/header/notification-inner/notification-inner.component';
import { ColorPickerModule } from 'ngx-color-picker';

@NgModule({
  declarations: [
    MenuComponent,
    SideNavComponent,
    MenuItemComponent,
    NotificationInnerComponent,
  ],
  imports: [
    CommonModule,
    MatMenuModule,
    AgGridModule,
    ColorPickerModule
  ]
})
export class SharedModule { }
