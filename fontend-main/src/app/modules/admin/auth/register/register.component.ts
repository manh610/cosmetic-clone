import { formatDate } from '@angular/common';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { notifi, routerNav } from 'src/app/core/models/constants';
import { AuthService } from 'src/app/core/services/auth.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { AuthStore } from 'src/app/core/stores/auth.store';

@Component({
  selector: 'app-register',
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.scss']
})
export class RegisterComponent implements OnInit, OnDestroy{
  private unsubscribe: Subscription[] = [];

  //#region VARIABLE
  registerFrm!: FormGroup;
  submitted = false;
  siteKey: string = "6LduN60nAAAAAK1Qb1RlA1wWhMrQRlsJF6MmDZjd";
  messages: string = "";
  error: boolean = false;

  genders = [{name: 'MALE', code: 'MALE'}, {name: 'FEMALE', code: 'FEMALE'}];
  //#endregion

  constructor(
    private fb: FormBuilder,
    private authStore: AuthStore,
    private router: Router,
    private _authService: AuthService,
    private _notifi: NotificationService
  ) {}
  ngOnInit(): void {
    this.createFrm();
  }

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  //#region FORM
  createFrm(){
    this.registerFrm = this.fb.group({
      username: ['', Validators.compose([Validators.required, Validators.pattern('[a-zA-Z0-9]{5,50}')])],
      email: ['',  Validators.compose([Validators.required, Validators.email])],
      phone: ['',  Validators.compose([Validators.required, Validators.pattern('[0-9]{10,15}')])],
      citizenNumber: ['', Validators.pattern('[0-9]{9,12}')],
      givenName: ['', Validators.compose([Validators.required, Validators.maxLength(15)])],
      familyName: ['', Validators.compose([Validators.required, Validators.maxLength(50)])],
      gender: ['MALE', Validators.required],
      dob: new FormControl(new Date()),
      country: ['VN'],
      roleId: ['01'],
      userRank: ['MEMBER'],
      avatar: [null],
      is2FA: [false],
      // password: ['']
      // recaptcha: ['', Validators.required]
    });
  }
  //#endregion

  //#region CRUD
  save(): void {
    try{
      const postReq =this.convertData();
      const sub = this._authService.register(postReq).subscribe((res: any) => {
        if (res.data) {
          this.messages = 'Đăng ký tài khoản thành công. Vui lòng truy cập email để kích hoạt tài khoản';
          this._notifi.showSuccess('Đăng ký tài khoản thành công. Vui lòng truy cập email để kích hoạt tài khoản', notifi.SUCCESS);
          this.error = false;
        }
      }, (error: any) => {
        this.error = true;
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.message.vn +' (' + e.message.en +').', notifi.INFO);
          // this.messages = this.messages + e.message.vn +' (' + e.message.en +').' +'\n';
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      console.error(ex);
    }
  }
  convertData() {
    const frmData = JSON.stringify(this.registerFrm.value, null, 4);
    try{
      const mydate = this.registerFrm.controls['dob'].value
      var formatedDate = new Date(mydate).toLocaleString();
      this.registerFrm.controls['dob'].setValue(formatDate(formatedDate, 'yyyy-MM-ddTHH:mm', 'en-US'));

      const frmData = JSON.stringify(this.registerFrm.value, null, 4);
      return frmData;
    }catch(ex) {
      console.error(ex);
      return frmData;
    }
  }
  //#endregion

  //#region ACTION
  onSubmit(): void {
    this.submitted = true;
    if (this.registerFrm.valid) {
      this.save();
      // this.scrollOnTop();
    }
  }
  //#endregion

  //#region EVENT
  scrollOnTop(): void {
    const element = document.getElementById('topOfPage');
    if(element) {
      element.scrollIntoView({behavior: 'smooth', block: 'start'});
    }
  }
  routerBack() {
    this.router.navigate([routerNav.NAV_LOGIN]);
  }
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.registerFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  //#endregion
}
