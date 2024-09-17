import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { notifi } from 'src/app/core/models/constants';
import { AddressService } from 'src/app/core/services/address.service';
import { NotificationService } from 'src/app/core/services/notification.service';

@Component({
  selector: 'app-address-item',
  templateUrl: './address-item.component.html',
  styleUrls: ['./address-item.component.scss']
})
export class AddressItemComponent implements OnInit, OnDestroy{
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();
  private unsubscribe: Subscription[] = [];

  addressFrm!: FormGroup;
  formType: any;
  address$: any;
  submitted: boolean = false;
  userId: any;

  province$: any = [];
  district$: any = [];
  ward$: any = [];

  isHome: boolean = true;

  constructor(private fb: FormBuilder,
    private route: ActivatedRoute,
    private _notifi: NotificationService,
    private addressService: AddressService
    ){
    }

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.initData();
  }

  initData() {
    this.initFrm();
    this.checkFrm();
  }

  //#region FORM
  initFrm(): void {
    this.addressFrm = this.fb.group({
      fullName: ['', Validators.required],
      phone: ['',  Validators.compose([Validators.required, Validators.pattern('[0-9]{9,15}')])],
      provinceId: ['', Validators.required],
      districtId: ['', Validators.required],
      wardId: ['', Validators.required],
      detail: [''],
      default: [false],
      addressType: [],
      userId: []
    })
  }
  checkFrm(): void {
    this.formType = this.dataDialog.formType;
    this.userId = this.dataDialog.userId;
    this.initProvinces();
    if(this.formType == 'edit') {
      this.address$ = this.dataDialog.address;
      this.addressFrm.patchValue(this.transformData(this.address$));
      if(this.address$.default) this.addressFrm.controls['default'].disable();
      if(this.address$.addressType == 'HOME') this.isHome = true;
      else this.isHome = false;
      this.initDistricts(this.address$.provinceId);
      this.initWard(this.address$.districtId);
    }
  }
  //#endregion

  //#region INIT
  initProvinces() {
    try{
      const sub = this.addressService.getProvinces('').subscribe((res: any) => {
        if(res.status) {
          this.province$ = res.data;
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  initDistricts(code: any) {
    try{
      const data: any = {
        provinceId: code
      }
      const sub = this.addressService.getDistricts(data).subscribe((res: any) => {
        if(res.status) {
          this.district$ = res.data;
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  initWard(code: any) {
    try{
      const data: any = {
        districtId: code
      }
      const sub = this.addressService.getWard(data).subscribe((res: any) => {
        if(res.status) {
          this.ward$ = res.data;
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  //#endregion

  //#region CRUD
  save(): void {
    try {
      if(this.formType == 'add') {
        let dataForm = this.addressFrm.value;
        dataForm.userId = this.userId;
        dataForm.addressType = this.isHome ? 'HOME' : 'WORK';
        const postData = JSON.stringify(dataForm, null, 4);
        const sub = this.addressService.addUser(postData).subscribe((res: any) => {
          if(res.status) {
            this.saveClicked.emit();
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
          }
        })
        this.unsubscribe.push(sub);
      }else {
        let dataForm = this.addressFrm.value;
        let data = this.transformData(dataForm);
        data.addressType = this.isHome ? 'HOME' : 'WORK';
        const sub = this.addressService.updateUser(data, data.id).subscribe((res: any) => {
          if(res.status) {
            this.saveClicked.emit();
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
          }
        })
        this.unsubscribe.push(sub);
      }
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  transformData(data:any){
    let oj = {
      id: this.address$.id,
      provinceId: data.provinceId,
      districtId: data.districtId,
      wardId: data.wardId,
      detail: data.detail,
      fullName: data.fullName,
      phone: data.phone,
      addressType: data.addressType,
      default: data.default,
      userId: this.userId,
      status: 'ACTIVE'
    }
    return oj;
  }
  //#endregion

  //#region EVENT
  onSubmit(): void {
    this.submitted = true;
    if (this.addressFrm.valid) {
      this.save();
    }
  }
  cancel(){
    this.cancelClicked.emit()
  }
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.addressFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  changeAddressType(number: any) {
    if(number == 1) this.isHome = true;
    else this.isHome = false;
  }
  selectProvince() {
    if(this.addressFrm.controls['provinceId'].value) {
      this.initDistricts(this.addressFrm.controls['provinceId'].value);
    }else {
      this.district$ = [];
    }
  }
  clearProvince() {
    this.addressFrm.controls['districtId'].setValue('');
    this.addressFrm.controls['wardId'].setValue('');
    this.district$ = [];
    this.ward$ = []
  }
  selectDistrict() {
    if(this.addressFrm.controls['districtId'].value) {
      this.initWard(this.addressFrm.controls['districtId'].value);
    }else {
      this.ward$ = [];
    }
  }
  clearDistrict() {
    this.addressFrm.controls['wardId'].setValue('');
    this.ward$ = []
  }
  //#endregion
}
