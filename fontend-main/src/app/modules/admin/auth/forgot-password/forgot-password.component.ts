import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { AuthService } from 'src/app/core/services/auth.service';
import { NotificationService } from 'src/app/core/services/notification.service';
import { Location } from '@angular/common';
import { notifi } from 'src/app/core/models/constants';

@Component({
  selector: 'app-forgot-password',
  templateUrl: './forgot-password.component.html',
  styleUrls: ['./forgot-password.component.scss']
})
export class ForgotPasswordComponent implements OnInit{
  private unsubscribe: Subscription[] = [];

  forgotPasswordForm!: FormGroup;
  submitted = false;
  isSuccess: boolean = false;
  isDisabled: boolean = false;

  constructor(private fb: FormBuilder,
    private _location: Location,
    private authService: AuthService,
    private route: ActivatedRoute,
    private _notifi: NotificationService) {
  }

  ngOnInit(): void {
    this.initForm();
  }

  initForm() {
    this.forgotPasswordForm = this.fb.group({
      email: ['', Validators.compose([Validators.required, Validators.email])],
    });
  }

  submit() {
    this.submitted = true;
    if(this.forgotPasswordForm.controls['email']?.value && this.forgotPasswordForm.valid) {
      try{
        this.isDisabled = true;
        const sub = this.authService.forgotPassword(this.forgotPasswordForm.controls['email']?.value).subscribe((res: any) => {
          if(res.data){
            this.isSuccess = true;
            this.isDisabled = false;
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            this._notifi.showInfo(e.message.vn +' (' + e.message.en +')', notifi.FAIL);
          }
          this.isDisabled = false;
        })
        this.unsubscribe.push(sub);
      }catch(e){
        this._notifi.showError(e, notifi.FAIL)
        console.error(e);
        this.isDisabled = false;
      }
    }
  }

  //#region EVENT
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.forgotPasswordForm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  backHistory() {
    this._location.back();
  }
  //#endregion
}
