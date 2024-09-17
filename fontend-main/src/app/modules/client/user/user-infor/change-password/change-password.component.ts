import { Component, OnInit, OnDestroy, Output, EventEmitter } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { CustomValidators } from 'src/app/core/_helpers/form.validator';
import { notifi } from 'src/app/core/models/constants';
import { StorageService } from 'src/app/core/services/auth/storage.service';
import { DataSharingService } from 'src/app/core/services/data-share.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { UserService } from 'src/app/core/services/user.service';

@Component({
  selector: 'app-change-password',
  templateUrl: './change-password.component.html',
  styleUrls: ['./change-password.component.scss']
})
export class ChangePasswordComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];
  @Output() tabChangeRequested = new EventEmitter<number>();

  receivedData: any;
  changePassFrm!: FormGroup;
  submitted = false;
  user: any;

  constructor(
    private _route: ActivatedRoute,
    private router: Router,
    private customValidator: CustomValidators,
    private fb: FormBuilder,
    private _notifi: NotificationService,
    private dataShare: DataSharingService,
    private storageService: StorageService,
    private userService: UserService
  ) {}
  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
    this.createFrm();
    this.initMatTabData();
  }

  //#region FORM
  initMatTabData(){
    this.dataShare.getData().subscribe(data => {
      this.receivedData = data;
    })
  }
  createFrm() {
    this.changePassFrm = this.fb.group({
      oldPassword: ['', Validators.required],
      newPassword: ['', Validators.compose([Validators.required, Validators.pattern(/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$/)])],
      confirmPassword: ['', Validators.compose([Validators.required])],
    }, {
      validators: [
        this.customValidator.MustMatch('newPassword', 'confirmPassword'),
        this.customValidator.PasswordMust('newPassword'),
      ]
    })
  }
  //#endregion

  //#region CRUD
  changePassword(): void{
    this.changePassFrm.markAllAsTouched();
    const formData = this.changePassFrm.getRawValue();
    const request: any = {
      newPassword: formData.newPassword,
      oldPassword: formData.oldPassword,
      username: this.receivedData.username,
    }
    try{
      const sub = this.userService.changePassword(request).subscribe((res: any) => {
        if(res.status) {
          this._notifi.showSuccess('Thay đổi mật khấu thành công', notifi.SUCCESS);
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(e){
      this._notifi.showError(e, notifi.FAIL);
    }
  }

  onSubmit(): void {
    this.submitted = true;
    if (this.changePassFrm.valid) {
      this.changePassword();
    }
  }
  back(): void {
    this.router.navigate(['/']);
  }
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.changePassFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  oldPasswordVisible: boolean = false;
  newPasswordVisible: boolean = false;
  cfmPasswordVisible: boolean = false;
  oldPasswordVisibility() {
    this.oldPasswordVisible = !this.oldPasswordVisible;
  }
  newPasswordVisibility() {
    this.newPasswordVisible = !this.newPasswordVisible;
  }
  cfmPasswordVisibility() {
    this.cfmPasswordVisible = !this.cfmPasswordVisible;
  }
  //#endregion
}
