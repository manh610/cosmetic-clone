import { NgModule } from '@angular/core';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatDividerModule } from '@angular/material/divider';
import { CommonModule } from '@angular/common';
import { OverlayModule } from '@angular/cdk/overlay';
import { CdkTreeModule } from '@angular/cdk/tree';
import { MatTreeModule } from '@angular/material/tree';
import { MatMenuModule } from '@angular/material/menu';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { ReactiveFormsModule } from '@angular/forms';
import { DropdownComponent } from './dropdown/dropdown.component';
import { TreeviewComponent } from './treeview/treeview.component';
@NgModule({
  imports: [
    CommonModule,
    MatIconModule,
    MatDividerModule,
    MatInputModule,
    CdkTreeModule,
    MatTreeModule,
    MatMenuModule,
    MatButtonToggleModule,
    MatCheckboxModule,
    MatDatepickerModule,
    MatNativeDateModule,
    ReactiveFormsModule,
  ],
  declarations: [
    DropdownComponent,
    TreeviewComponent
  ],
  exports: [
    MatInputModule,
    OverlayModule,
    DropdownComponent,
    TreeviewComponent,
  ]
})
export class CustomCommonModule { }
