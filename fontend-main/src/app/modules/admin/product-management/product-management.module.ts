import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import {MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule, MatOptionModule } from '@angular/material/core';
import { TranslateModule } from '@ngx-translate/core';
import { MatIconModule } from '@angular/material/icon';
import {MatSlideToggleModule} from '@angular/material/slide-toggle';
import { TableModule } from 'primeng/table';
import { DropdownModule } from 'primeng/dropdown';
import { PaginatorModule } from 'primeng/paginator';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MAT_SELECT_SCROLL_STRATEGY_PROVIDER, MatSelectModule } from '@angular/material/select';
import { NgxMatSelectSearchModule } from 'ngx-mat-select-search';
import {MatAutocompleteSelectedEvent, MatAutocompleteModule, MAT_AUTOCOMPLETE_SCROLL_STRATEGY} from '@angular/material/autocomplete';
import { CustomCommonModule } from 'src/app/common/common.module';
import { ToastrModule } from 'ngx-toastr';
import { ButtonModule } from 'primeng/button';
import { OptionAttributeComponent } from './option-attribute/option-attribute.component';
import { ProductManagementRoutingModule } from './product-management-routing.module';
import { OptionAttributeItemComponent } from './option-attribute/option-attribute-item/option-attribute-item.component';
import { MatChipInputEvent, MatChipsModule } from '@angular/material/chips';
import { ColorPickerModule } from 'ngx-color-picker';
import { ProductComponent } from './product/product.component';
import { ProductItemComponent } from './product/product-item/product-item.component';
import { CalendarModule } from 'primeng/calendar';
import { MultiSelectModule } from 'primeng/multiselect';
import { TriStateCheckboxModule } from 'primeng/tristatecheckbox';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { ChipsModule } from 'primeng/chips';
import { NgxEditorModule } from 'ngx-editor';
import { ValueDetailComponent } from './product/value-detail/value-detail.component';
import { FileUploadModule } from 'primeng/fileupload';
import { BadgeModule } from 'primeng/badge';

@NgModule({
  declarations: [
    OptionAttributeComponent,
    OptionAttributeItemComponent,
    ProductComponent,
    ProductItemComponent,
    ValueDetailComponent
  ],
  imports: [
    CommonModule,
    ProductManagementRoutingModule,
    FormsModule,
    ReactiveFormsModule,
    MatDatepickerModule,
    MatNativeDateModule,
    TranslateModule,
    MatIconModule,
    MatSlideToggleModule,
    TableModule,
    FormsModule,
    DropdownModule,
    PaginatorModule,
    MatFormFieldModule,
    MatSelectModule,
    MatAutocompleteModule,
    NgxMatSelectSearchModule,
    CalendarModule,
    MultiSelectModule,
    CustomCommonModule,
    ToastrModule,
    ButtonModule,
    MatChipsModule,
    MatOptionModule,
    ColorPickerModule,
    TriStateCheckboxModule,
    MatCheckboxModule,
    ChipsModule,
    NgxEditorModule,
    FileUploadModule,
    BadgeModule
  ],
  providers: [
    MatDatepickerModule,
    MatNativeDateModule
  ],
})
export class ProductManagementModule { }
