import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ClientRoutingModule } from './client-routing.module';
import { ClientComponent } from './client.component';
import { HomeComponent } from './home/home.component';
import { CarouselModule } from 'primeng/carousel';
import { ButtonModule } from 'primeng/button';
import { MatTabsModule } from '@angular/material/tabs';
import { MatMenuModule} from '@angular/material/menu';
import { MatIconModule } from '@angular/material/icon';
import { MdbTabsModule } from 'mdb-angular-ui-kit/tabs';
import { MenuItemComponent } from 'src/app/shared/client-layout/menu-item/menu-item.component';
import { FlipClockComponent } from 'src/app/shared/client-layout/flip-clock/flip-clock.component';
import { CarouselComponent } from 'src/app/shared/client-layout/carousel/carousel.component';
import { FooterComponent } from 'src/app/shared/client-layout/footer/footer.component';
import { HeaderComponent } from 'src/app/shared/client-layout/header/header.component';
import { TreeSelectModule } from 'primeng/treeselect';
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { CdkTreeModule } from '@angular/cdk/tree';
import { MatTreeModule } from '@angular/material/tree';
import { CustomCommonModule } from 'src/app/common/common.module';
import { CategoryModule } from './category/category.module';
import { UserModule } from './user/user.module';
import { ProductComponent } from './product/product.component';
import { SearchModule } from './search/search.module';
import { RatingModule } from 'primeng/rating';
import { InputNumberModule } from 'primeng/inputnumber';
import { BadgeModule } from 'primeng/badge';
import { CartInnerComponent } from 'src/app/shared/client-layout/header/cart-inner/cart-inner.component';
import { OverlayPanelModule } from 'primeng/overlaypanel';
import { NgxPaginationModule } from 'ngx-pagination';
import { CartComponent } from './cart/cart.component';
import { TableModule } from 'primeng/table';
import { CheckboxModule } from 'primeng/checkbox';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { FavoriteInnerComponent } from 'src/app/shared/client-layout/header/favorite-inner/favorite-inner.component';
import { OrderModule } from './order-product/order-product.module';

@NgModule({
  declarations: [
    ClientComponent,
    HeaderComponent,
    FooterComponent,
    CarouselComponent,
    FlipClockComponent,
    MenuItemComponent,
    HomeComponent,
    ProductComponent,
    CartInnerComponent,
    CartComponent,
    FavoriteInnerComponent,
  ],
  imports: [
    CommonModule,
    ClientRoutingModule,
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
    CategoryModule,
    UserModule,
    SearchModule,
    RatingModule,
    InputNumberModule,
    BadgeModule,
    OverlayPanelModule,
    NgxPaginationModule,
    TableModule,
    CheckboxModule,
    MatCheckboxModule,
    OrderModule
  ],
})
export class ClientModule { }
