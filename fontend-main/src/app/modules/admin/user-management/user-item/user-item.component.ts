import { Component, OnInit, OnDestroy } from '@angular/core';
import { Observable, ReplaySubject, Subject, Subscription, map, startWith, takeUntil } from 'rxjs';
import { FormBuilder, FormControl, FormGroup, FormArray,Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { NotificationService } from 'src/app/core/services/notification.service';
import { TranslationService } from 'src/app/modules/i18n';
import { formatDate } from '@angular/common';
import { UserService } from 'src/app/core/services/user.service';
import { Location } from '@angular/common';
import { notifi, routerNav } from 'src/app/core/models/constants';
import { AddressService } from 'src/app/core/services/address.service';
import { CustomValidators } from 'src/app/core/_helpers/form.validator';

@Component({
  selector: 'app-user-item',
  templateUrl: './user-item.component.html',
  styleUrls: ['./user-item.component.scss']
})
export class UserItemComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];
  //#region VARIABLE
  form!: FormGroup;
  countries: any = [{
    code: 'ad',
    name: 'Việt Nam'
  },{
    code: 'am',
    name: 'Anh'
  }]
  selectedCountry: any | undefined;
  userFrm!: FormGroup;
  submitted = false;
  formId:any;
  formType:any;
  user$: any;
  provinces$: any;
  districts$: any;
  isEdit = true;
  //#endregion
  constructor(
    private _route: ActivatedRoute,
    private router: Router,
    private customValidator: CustomValidators,
    private fb: FormBuilder,
    private _location: Location,
    private _notifi: NotificationService,
    private translation: TranslationService,
    private userService: UserService,
    private addressService: AddressService
  ) {}
  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.form = this.fb.group({
      address: this.fb.array([]),
    });
    this.createFrm();
    this.checkFrm();
    this.initProvinces();
  }

  //#region FORM
  createFrm() {
    this.userFrm = this.fb.group({
      username: ['', Validators.compose([Validators.required, Validators.pattern('[a-zA-Z0-9]{5,50}')])],
      password: ['', Validators.compose([Validators.required, Validators.pattern(/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$/)])],
      email: ['',  Validators.compose([Validators.required, Validators.email])],
      phone: ['',  Validators.compose([Validators.required, Validators.pattern('[0-9]{10,15}')])],
      citizenNumber: ['', Validators.pattern(/^(\d{9}|\d{12})$/)],
      givenName: ['', Validators.compose([Validators.required, Validators.maxLength(15)])],
      familyName: ['', Validators.compose([Validators.required, Validators.maxLength(50)])],
      gender: ['', Validators.required],
      dob: new FormControl(new Date()),
      country: [''],
      roleId: [''],
      status: [''],
      userRank: [''],
      avatar: [null],
      is2FA: [false],
      description: [''],

      confirmPassword: ['', Validators.compose([Validators.required])]
    }, {
      validators: [
        this.customValidator.MustMatch('password', 'confirmPassword')
      ]
    });
  }
  checkFrm(): void {
    try{
      this.formId = this._route.snapshot.paramMap.get('id');
      this.formType = this._route.snapshot.paramMap.get('type');
      if(this.formType == 'edit') {
        this.isEdit = false;
        this.userFrm.controls['username'].disable();
        this.userFrm.controls['status'].disable();
        this.userFrm.controls['roleId'].disable();
        this.userFrm.controls['password'].disable();
        this.userFrm.controls['confirmPassword'].disable();
        this.initUser();
      } else if(this.formType == 'detail') {
        this.isEdit = true;
        this.userFrm.disable();
        this.initUser();
      } else {
        this.isEdit = true;
        this.userFrm.controls['gender'].setValue('MALE');
        this.userFrm.controls['country'].setValue('VN');
        this.userFrm.controls['userRank'].setValue('MEMBER');
        this.userFrm.controls['roleId'].setValue('01');
        this.userFrm.controls['status'].setValue('ACTIVE');
      }
    }catch(ex){
      this._notifi.showInfo(ex, notifi.FAIL);
    }
  }
  //#endregion

  //#region CRUD
  initUser() {
    try{
      const sub = this.userService.getById(this.formId).subscribe((res: any) => {
        if(res.status){
          this.user$ = res.data;
          this.userFrm.patchValue(this.transformData(res.data));
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      console.error(ex);
    }
  }
  initProvinces() {
    try{
      let request: any = {
        name: ''
      }
      const sub = this.addressService.getProvinces(request).subscribe((res: any) => {
        if(res.status) {
          this.provinces$ = res.data;
        }
      }, (error: any) => {

      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      console.error(ex);
    }
  }

  initDistricts() {
    try{
      let request: any = {
        provinceId: '02'
      }
      const sub = this.addressService.getDistricts(request).subscribe((res: any) => {
        if(res.status) {
          this.districts$ = res.data;
        }
      }, (error) => {

      })
      this.unsubscribe.push(sub);
    }catch(ex){
      console.error(ex);
    }
  }
  save(): void {
    try{
      if(this.formType == 'add') {
        const postData = this.convertData();
        const sub = this.userService.create(postData).subscribe((res: any) => {
          if (res.status) {
            this._notifi.showSuccess('Thêm mới người dùng '+res.data.username+' thành công', notifi.SUCCESS);
            this.router.navigate([routerNav.NAV_USER]);
          } else {
            this._notifi.showError('Thêm mới người dùng thất bại', notifi.FAIL);
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
          }
        })
        this.unsubscribe.push(sub);
      }else {
        let dataForm = this.userFrm.value;
        let data = this.transformData(dataForm);
        const sub = this.userService.update(data).subscribe((res: any) => {
          if(res.status) {
            this._notifi.showSuccess('Cập nhật người dùng '+res.data.username+' thành công', notifi.SUCCESS);
            this.router.navigate([routerNav.NAV_USER]);
          }else {
            this._notifi.showError('Cập nhật người dùng '+res.data.username+' thất bại', notifi.FAIL);
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
          }
        })
        this.unsubscribe.push(sub);
      }
    }catch(ex){
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  convertData() {
    const frmData = JSON.stringify(this.userFrm.value, null, 4);
    try{
      //convertData
      const mydate = this.userFrm.controls['dob'].value
      var formatedDate = new Date(mydate).toLocaleString();
      this.userFrm.controls['dob'].setValue(formatDate(formatedDate, 'yyyy-MM-ddTHH:mm', 'en-US'));

      const frmData = JSON.stringify(this.userFrm.value, null, 4);
      return frmData;
    }catch(ex){
      return frmData;
    }
  }
  transformData(data:any){
    let oj = {
      id: this.user$.id,
      username: this.user$.username,
      password: '123456aA@',
      confirmPassword: this.user$.password,
      email: data.email,
      phone: data.phone,
      givenName: data.givenName,
      familyName: data.familyName,
      citizenNumber: data.citizenNumber,
      gender: data.gender,
      dob: data.dob,
      country: data.country,
      userRank: data.userRank,
      avatar: data.avatar,
      roleId: this.user$.roleId,
      status: 'ACTIVE'
    }
    return oj;
  }
  //#endregion

  //#region ACTION
  onSubmit(): void {
    this.submitted = true;
    if (this.userFrm.valid) {
      this.save();
    }
  }
  back() {
    this._location.back();
  }
  //#endregion

  //#region EVENT
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.userFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  //#endregion


  // Thêm một control mới vào FormArray
  addControl() {
    const control = this.fb.group({
      code: ['', Validators.compose([Validators.required, Validators.pattern('[a-zA-Z0-9]{3,50}')])],
      provinceId: ['', Validators.required],
      districtId: [''],
      wardId: [''],
      detail: [''],
      isDefault: []
    });
    (this.form.get('address') as FormArray).push(control);
  }
  // Xóa control khỏi FormArray
  removeControl(index: number) {
    (this.form.get('address') as FormArray).removeAt(index);
  }
  // Lấy ra FormArray để sử dụng trong template
  get addressArrayControls() {
    return (this.form.get('address') as FormArray).controls;
  }
}
