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
import { SearchComponent } from './search.component';
import { SearchRoutingModule } from './search-routing.module';
import { RadioButtonModule } from 'primeng/radiobutton';
import { SliderModule } from 'primeng/slider';

@NgModule({
  declarations: [
    SearchComponent
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
    SearchRoutingModule,
    RadioButtonModule,
    SliderModule
  ]
})
export class SearchModule { }
