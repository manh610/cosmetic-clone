import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatIconModule } from '@angular/material/icon';

import { FormsModule } from '@angular/forms';
// import { NgxChartsModule } from '@swimlane/ngx-charts';

import { DashboardComponent } from './dashboard.component';
import { ChartModule } from 'primeng/chart';
import { OrderItemComponent } from './order-item/order-item.component';
@NgModule({
  declarations: [
    DashboardComponent,
    OrderItemComponent
  ],
  imports: [
    CommonModule,
    MatIconModule,
    FormsModule,
    ChartModule,
    // NgxChartsModule
  ]
})
export class DashboardModule { }
