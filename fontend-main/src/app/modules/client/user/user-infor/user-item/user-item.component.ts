import { Component, EventEmitter, OnDestroy, OnInit, Output, ViewEncapsulation } from '@angular/core';
import { Subscription } from 'rxjs';
import { constants, notifi } from 'src/app/core/models/constants';
import { StorageService } from 'src/app/core/services/auth/storage.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { UserService } from 'src/app/core/services/user.service';
import { FormBuilder, FormControl, FormGroup, FormArray,Validators } from '@angular/forms';
import { formatDate } from '@angular/common';
import { Router } from '@angular/router';
import { DataSharingService } from 'src/app/core/services/data-share.service';

@Component({
  selector: 'app-user-item',
  templateUrl: './user-item.component.html',
  styleUrls: ['./user-item.component.scss']
})
export class UserItemComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];
  @Output() tabChangeRequested = new EventEmitter<number>();

  date!: Date;

  frm!: FormGroup;
  selectedFile!: File;
  imageSrc: any;
  user: any;
  submitted: boolean = false;
  constructor(private fb: FormBuilder,
    private router: Router,
    private _notifi: NotificationService,
    private fnConstants: constants,
    private dataShare: DataSharingService,
    private storageService: StorageService,
    private userService: UserService)
  {
    this.createFrm();
  }

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.frm.controls['username'].disable();
    this.frm.controls['userRank'].disable();
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.user = currentUser;
      this.dataShare.setData(this.user);
      this.frm.patchValue(this.transformData(currentUser));
      this.imageSrc = this.user.avatar ?'data:image/jpg;base64,' + this.user.avatar : null;
    }
  }

  createFrm() {
    this.frm = this.fb.group({
      username: ['', Validators.compose([Validators.required, Validators.pattern('[a-zA-Z0-9]{5,50}')])],
      email: ['',  Validators.compose([Validators.required, Validators.email])],
      phone: ['',  Validators.compose([Validators.required, Validators.pattern('[0-9]{10,15}')])],
      citizenNumber: ['', Validators.pattern('[0-9]{9,12}')],
      givenName: ['', Validators.compose([Validators.required, Validators.maxLength(15)])],
      familyName: ['', Validators.compose([Validators.required, Validators.maxLength(50)])],
      gender: ['', Validators.required],
      dob: new FormControl(new Date()),
      country: [''],
      avatar: [null],
      userRank: [''],
      description: [''],
    });
  }

  //#region INIT
  convertData() {
    const frmData = JSON.stringify(this.frm.value, null, 4);
    try{
      //convertData
      const mydate = this.frm.controls['dob'].value
      var formatedDate = new Date(mydate).toLocaleString();
      this.frm.controls['dob'].setValue(formatDate(formatedDate, 'yyyy-MM-ddTHH:mm', 'en-US'));

      const frmData = JSON.stringify(this.frm.value, null, 4);
      return frmData;
    }catch(ex){
      return frmData;
    }
  }
  transformData(data:any){
    let oj = {
      id: this.user.id,
      username: this.user.username,
      password: '123456aA@',
      email: data.email,
      phone: data.phone,
      givenName: data.givenName,
      familyName: data.familyName,
      citizenNumber: data.citizenNumber,
      gender: data.gender,
      dob: data.dob,
      country: data.country,
      userRank: this.user.userRank,
      avatar: data.avatar,
      roleId: this.user.roleId,
      status: this.user.status
    }
    return oj;
  }
  //#endregion

  //#region ACTION
  save(): void {
    try{
      let dataForm = this.frm.value;
        let data = this.transformData(dataForm);
        if(this.imageSrc) {
          const [, base64Data] = this.imageSrc.split(',');
          data.avatar = base64Data;
        }
        const sub = this.userService.update(data).subscribe((res: any) => {
          if(res.status) {
            localStorage.setItem("currentUser", JSON.stringify(res.data));
            this._notifi.showSuccess('Cập nhật thông tin cá nhân thành công', notifi.SUCCESS);
          }else {
            this._notifi.showError('Cập nhật thông tin cá nhân thất bại', notifi.FAIL);
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            console.error(e.code + '-' + e.message.en, notifi.FAIL);
          }
        })
        this.unsubscribe.push(sub);
    }catch(ex){
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  onSubmit(): void {
    this.submitted = true;
    if (this.frm.valid) {
      this.save();
    }
  }

  onFileSelected(event: any): void {
    this.selectedFile = event.target.files?.[0] || null;
    if (this.selectedFile) {
      const allowedExtensions = ['jpg', 'jpeg', 'png', 'gif', 'svg'];
      const fileName = this.selectedFile.name.toLowerCase();
      const fileExtension: any = fileName.split('.').pop();
      if (allowedExtensions.includes(fileExtension)) {
        const reader = new FileReader();
        reader.onload = (e) => {
          this.imageSrc = e.target?.result;
        };
        reader.readAsDataURL(this.selectedFile!);
      } else {
        this._notifi.showInfo('Định dạng File không hợp lệ', notifi.INFO);
      }
    }
  }
  back(): void {
    this.router.navigate(['/']);
  }
  //#endregion

  //#region EVENT
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.frm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  //#endregion
}
