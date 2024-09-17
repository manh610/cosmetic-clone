import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import {MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { TranslateModule } from '@ngx-translate/core';
import { MatIconModule } from '@angular/material/icon';
import {MatSlideToggleModule} from '@angular/material/slide-toggle';
import {MatTooltipModule} from '@angular/material/tooltip';
import { MatChipInputEvent, MatChipsModule } from '@angular/material/chips';
import {
  MatAutocompleteSelectedEvent,
  MatAutocompleteModule,
} from '@angular/material/autocomplete';
import { TableModule } from 'primeng/table';
import { DropdownModule } from 'primeng/dropdown';
import { PaginatorModule } from 'primeng/paginator';
import { NgxPaginationModule } from 'ngx-pagination';
import { DiscountManagementComponent } from './discount-management.component';
import { DiscountItemComponent } from './discount-item/discount-item.component';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { DiscountSendComponent } from './discount-send/discount-send.component';
import { MatTabsModule } from '@angular/material/tabs';
import { DiscountVariantComponent } from './discount-send/discount-variant/discount-variant.component';

@NgModule({
  declarations: [
    DiscountManagementComponent,
    DiscountItemComponent,
    DiscountSendComponent,
    DiscountVariantComponent,
  ],
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    MatDatepickerModule,
    MatNativeDateModule,
    TranslateModule,
    MatIconModule,
    MatSlideToggleModule,
    MatTooltipModule,
    MatChipsModule,
    MatAutocompleteModule,
    TableModule,
    FormsModule,
    DropdownModule,
    PaginatorModule,
    NgxPaginationModule,
    MatCheckboxModule,
    MatTabsModule
  ],
  providers: [
    MatDatepickerModule,
    MatNativeDateModule
  ],
})
export class DiscountManagementModule { }
