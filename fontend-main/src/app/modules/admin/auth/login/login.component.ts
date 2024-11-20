import { Component, OnDestroy, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { constants, notifi, routerNav } from 'src/app/core/models/constants';
import { AuthService } from 'src/app/core/services/auth.service';
import { StorageService } from 'src/app/core/services/auth/storage.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { UserService } from 'src/app/core/services/user.service';
import { AuthStore } from 'src/app/core/stores/auth.store';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  //#region VARIABLE
  loginFrm!: FormGroup;
  passwordVisible: boolean = false;
  submitted = false;
  role: any;
  //#endregion

  constructor(
    private fb: FormBuilder,
    private authStore: AuthStore,
    private router: Router,
    private _authService: AuthService,
    private storageService: StorageService,
    private _notifi: NotificationService,
    private fnConstant: constants,
    private userService: UserService
  ) {}

  ngOnInit(): void {
    this.initForm();
  }

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }

  initForm(){
    this.loginFrm = this.fb.group({
      username: ['', Validators.required],
      password: ['', Validators.required],
    });
  }
  //#region ACTION
  onSubmit(): void {
    this.submitted = true;
    if (this.loginFrm.valid) {
      // this.doLogin();
      this.router.navigate(['/']);
    }
  }
  doLogin(): void{
    this.loginFrm.markAllAsTouched();
    const formData = this.loginFrm.getRawValue();
    const request: any = {
      username: formData.username,
      password: formData.password,
    }
    try{
      const sub = this._authService.requestAccessToken(request).subscribe((res: any) => {
        if(res) {
          this.getCurrentUser(res);
          setTimeout(() => {
            this._notifi.showSuccess('Đăng nhập thành công', notifi.SUCCESS);
            this.router.navigate(['/']);
          }, 50);
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(err) {
      console.error(err);
    }
  }

  getCurrentUser(accessToken: any) {
    if(accessToken) {
      const data: any = {
        accessToken: accessToken.jwtToken
      }
      try{
        const sub = this.userService.getUserFromToken(data).subscribe((res: any) => {
          if(res.status) {
            this.storageService.saveUser(res.data);
            localStorage.setItem("currentUser", JSON.stringify(res.data));
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            this._notifi.showInfo(e, notifi.FAIL);
          }
        })
      }catch(ex){
        this._notifi.showError(ex, notifi.FAIL);
      }
    }
  }
  routerRegister(): void {
    this.router.navigate([routerNav.NAV_REGISTER]);
  }
  routerForgotPassword(): void {
    this.router.navigate([routerNav.NAV_FORGOT_PASSWORD]);
  }
  //#endregion
  //#region EVENT
  passwordVisibility() {
    this.passwordVisible = !this.passwordVisible;
  }
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.loginFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  //#endregion
}
