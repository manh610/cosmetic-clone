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
import { CalendarModule } from 'primeng/calendar';
import { MatRadioModule } from '@angular/material/radio';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { DropdownModule } from 'primeng/dropdown';
import { OrderProductComponent } from './order-product.component';
import { AddressComponent } from './address/address.component';
import { RadioButtonModule } from 'primeng/radiobutton';
import { TableModule } from 'primeng/table';
import { DiscountComponent } from './discount/discount.component';

@NgModule({
  declarations: [
    OrderProductComponent,
    AddressComponent,
    DiscountComponent
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
    MatTabsModule,
    MatDatepickerModule,
    MatNativeDateModule,
    MatCheckboxModule,
    DropdownModule,
    RadioButtonModule,
    TableModule
  ],
  providers: [
    MatDatepickerModule,
    MatNativeDateModule
  ],
})
export class OrderModule { }
