import { Component, EventEmitter, ElementRef, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { AbstractControl, FormBuilder, FormGroup, FormArray, FormControl, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { Subscription, ReplaySubject, Subject, takeUntil } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { icons, notifi } from 'src/app/core/models/constants';
import { AttributeService } from 'src/app/core/services/product/option-attribute.service';
import { OptionDetailService } from 'src/app/core/services/product/option-detail.service';
import { MatChipInputEvent } from '@angular/material/chips';

@Component({
  selector: 'app-option-attribute-item',
  templateUrl: './option-attribute-item.component.html',
  styleUrls: ['./option-attribute-item.component.scss']
})
export class OptionAttributeItemComponent implements OnInit, OnDestroy {
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();

  attributeFrm!: FormGroup;
  attribute: any;
  formType: any;
  formId: any;
  submitted = false;

  color: any;
  cntDetail: number = 0;
  // optionDetail: any;

  optionDetails: any = [];
  optionDetailOrigin: any = [];
  frmCtrl = new FormControl();
  @ViewChild('optionInput') optionInput: ElementRef<HTMLInputElement> | any;

  private unsubscribe: Subscription[] = [];
  constructor(private fb: FormBuilder,
    private route: ActivatedRoute,
    private _notifi: NotificationService,
    private attributeService: AttributeService,
    private optionDetailService: OptionDetailService
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
      this.attributeFrm = this.fb.group({
        code: ['', [Validators.required, Validators.pattern('[a-zA-Z0-9._-]{3,255}')]],
        name: ['', Validators.required],
        status: [''],
        description: ['']
      })
    }
    checkFrm(): void {
      this.formType = this.dataDialog.formType;
      if(this.formType == 'edit') {
        this.formId = this.dataDialog.id;
        this.attributeFrm.controls['code'].disable();
        this.getById();
      }
    }
    //#endregion

    //#region INIT
    getById() {
      try{
        const sub = this.attributeService.getById(this.formId).subscribe((res: any) => {
          if(res.status) {
            this.attribute = res.data;
            // if(res.data.optionDetailResponse.length>0) {
            //   this.optionDetailOrigin = res.data.optionDetailResponse.map((x: any) => x.value);
            //   this.optionDetails = res.data.optionDetailResponse.map((x: any) => x.value);
            // }
            this.attributeFrm.patchValue(this.transformData(res.data));
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
          let dataForm = this.attributeFrm.value;
          dataForm.status = 'ACTIVE';
          const postData = JSON.stringify(dataForm, null, 4);
          const sub = this.attributeService.create(postData).subscribe((res: any) => {
            if(res.status) {
              // this.optionDetails.forEach((item: any) => {
              //   if (item != '') this.addOptionDetail(res.data.id, item);
              // })
              this.saveClicked.emit();
            }
          }, (error: any) => {
            for (let e of error.error.errors) {
              this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
            }
          })
          this.unsubscribe.push(sub);
        }else {
          debugger
          let dataForm = this.attributeFrm.value;
          let data = this.transformData(dataForm);
          const sub = this.attributeService.update(this.formId, data).subscribe((res: any) => {
            if(res.status) {
              // this.removeAttribute(res.data.id);
              // this.optionDetails.forEach((item: any) => {
              //   if (item != '') this.addOptionDetail(res.data.id, item);
              // })
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

    // addOptionDetail(optionAttributeId: any, value: any) {
    //   try {
    //     const data: any = {
    //       optionAttributeId: optionAttributeId,
    //       value: value
    //     }
    //     const sub = this.optionDetailService.add(data).subscribe((res: any) => {
    //       if(res.status) {
    //         // this.cntDetail = this.cntDetail + 1;
    //       } else {
    //         this._notifi.showInfo('Thất bại khi thêm giá trị ' + value+ 'cho thuộc tính', notifi.INFO);
    //       }
    //     }, (error: any) => {
    //       for (let e of error.error.errors) {
    //         this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
    //       }
    //     })
    //     this.unsubscribe.push(sub);
    //   }catch(ex) {
    //     this._notifi.showError(ex, notifi.FAIL);
    //   }
    // }

    // removeAttribute(id: string) {
    //   try{
    //     const sub = this.optionDetailService.removeByAttribute(id).subscribe((res: any) => {
    //     }, (error: any) => {
    //       for (let e of error.error.errors) {
    //         this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
    //       }
    //     })
    //     this.unsubscribe.push(sub);
    //   }catch(ex) {
    //     this._notifi.showError(ex, notifi.FAIL);
    //   }
    // }

    transformData(data:any){
      let oj = {
        id: this.attribute.id,
        code: this.attribute.code,
        name: data.name,
        description: data.description,
        status: this.attribute.status
      }
      return oj;
    }
    //#endregion

    //#region ACTION
    onSubmit(): void {
      this.submitted = true;
      if (this.attributeFrm.valid) {
        // this.optionDetails = [...new Set(this.optionDetails)];
        this.save();
      }
    }
    cancel(){
      this.cancelClicked.emit()
    }
    //#endregion

    //#region EVENT
    hasErrorInput(controlName: string, errorName: string): boolean {
      const control = this.attributeFrm.get(controlName);
      if (control == null) {
        return false;
      }
      return (control.dirty || control.touched) && control.hasError(errorName);
    }
    //#endregion

  addGr(event: MatChipInputEvent) {
    const value = (event.value || '').trim();
    this.optionDetails.push(value);
    event.chipInput!.clear();
    this.frmCtrl.setValue(null);
  }
  removeGr(group: any): void {
    const index = this.optionDetails.indexOf(group);
    if (index >= 0) {
      this.optionDetails.splice(index, 1);
    }
  }
}
