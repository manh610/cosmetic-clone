import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { OptionAttributeComponent } from './option-attribute/option-attribute.component';
import { ProductComponent } from './product/product.component';
import { ProductItemComponent } from './product/product-item/product-item.component';

const routes: Routes = [
  {
    path: '',
    component: ProductComponent,
    title: 'Quản lý sản phẩm'
  },
  {
    path: 'attribute',
    component: OptionAttributeComponent,
    title: 'Quản lý thuộc tính sản phẩm'
  },
  {
    path: 'product-item/:id/:type',
    component: ProductItemComponent,
  }
];
@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ProductManagementRoutingModule { }
