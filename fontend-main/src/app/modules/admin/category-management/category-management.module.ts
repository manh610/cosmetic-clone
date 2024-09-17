import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import {MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { TranslateModule } from '@ngx-translate/core';
import { MatIconModule } from '@angular/material/icon';
import {MatSlideToggleModule} from '@angular/material/slide-toggle';
import { TableModule } from 'primeng/table';
import { DropdownModule } from 'primeng/dropdown';
import { PaginatorModule } from 'primeng/paginator';
import { CategoryManagementComponent } from './category-management.component';
import { CategoryItemComponent } from './category-item/category-item.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { NgxMatSelectSearchModule } from 'ngx-mat-select-search';
import { TreeSelectModule } from 'primeng/treeselect';
import {
  MatAutocompleteSelectedEvent,
  MatAutocompleteModule,
} from '@angular/material/autocomplete';
import { CustomCommonModule } from 'src/app/common/common.module';
import { ToastrModule } from 'ngx-toastr';
import { ButtonModule } from 'primeng/button';

@NgModule({
  declarations: [
    CategoryManagementComponent,
    CategoryItemComponent,
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
    TableModule,
    FormsModule,
    DropdownModule,
    PaginatorModule,
    MatFormFieldModule,
    MatSelectModule,
    MatAutocompleteModule,
    NgxMatSelectSearchModule,
    CustomCommonModule,
    ToastrModule,
    ButtonModule,
    TreeSelectModule
  ],
})
export class CategoryManagementModule { }
