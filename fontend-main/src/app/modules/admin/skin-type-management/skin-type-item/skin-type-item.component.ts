import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { AbstractControl, FormBuilder, FormGroup, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { Subscription, ReplaySubject, Subject, takeUntil } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { icons, notifi } from 'src/app/core/models/constants';
import { SkinTypeService } from 'src/app/core/services/skin-type.service';

@Component({
  selector: 'app-skin-type-item',
  templateUrl: './skin-type-item.component.html',
  styleUrls: ['./skin-type-item.component.scss']
})
export class SkinTypeItemComponent implements OnInit, OnDestroy {
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();

  skinTypeFrm!: FormGroup;
  skinType: any;
  formType: any;
  formId: any;
  submitted = false;

  private unsubscribe: Subscription[] = [];
  constructor(private fb: FormBuilder,
    private route: ActivatedRoute,
    private _notifi: NotificationService,
    private _Service: SkinTypeService
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
    this.skinTypeFrm = this.fb.group({
      name: ['', Validators.required],
      description: ['']
    })
  }
  checkFrm(): void {
    this.formType = this.dataDialog.formType;
    if(this.formType == 'edit') {
      this.formId = this.dataDialog.id;
      this.getById();
    }
  }
  //#endregion

  //#region INIT
  getById() {
    try{
      const sub = this._Service.getById(this.formId).subscribe((res: any) => {
        if(res.status) {
          this.skinType = res.data;
          this.skinTypeFrm.patchValue(this.transformData(res.data));
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
        let dataForm = this.skinTypeFrm.value;
        const postData = JSON.stringify(dataForm, null, 4);
        const sub = this._Service.create(postData).subscribe((res: any) => {
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
        let dataForm = this.skinTypeFrm.value;
        let data = this.transformData(dataForm);
        const sub = this._Service.update(this.formId, data).subscribe((res: any) => {
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
      id: this.skinType.id,
      name: data.name,
      description: data.description
    }
    return oj;
  }
  //#endregion

  //#region ACTION
  onSubmit(): void {
    this.submitted = true;
    if (this.skinTypeFrm.valid) {
      this.save();
    }
  }
  cancel(){
    this.cancelClicked.emit()
  }
  //#endregion

  //#region EVENT
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.skinTypeFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  //#endregion
}
