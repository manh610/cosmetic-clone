import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AuthComponent } from './auth.component';
import { RegisterComponent } from './register/register.component';
import { ActiveComponent } from './active/active.component';
import { ForgotPasswordComponent } from './forgot-password/forgot-password.component';

const routes: Routes = [
  {
    path: 'login',
    component: AuthComponent,
    title: 'Đăng nhập',
  },
  {
    path: 'register',
    component: RegisterComponent,
    title: 'Đăng Ký Tài Khoản',
  },
  {
    path: 'forgot-password',
    component: ForgotPasswordComponent,
    title: 'Quên mật khẩu',
  },
  {
    path: 'active',
    component: ActiveComponent,
    title: 'Kích hoạt tài khoản'
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class AuthRoutingModule { }
