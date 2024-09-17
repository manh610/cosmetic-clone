import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { AbstractControl, FormBuilder, FormGroup, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { Subscription, ReplaySubject, Subject, takeUntil } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { icons, notifi } from 'src/app/core/models/constants';
import { BrandService } from 'src/app/core/services/brand.service';

@Component({
  selector: 'app-brand-item',
  templateUrl: './brand-item.component.html',
  styleUrls: ['./brand-item.component.scss']
})
export class BrandItemComponent implements OnInit, OnDestroy {
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();

  brandFrm!: FormGroup;
  brand: any;
  formType: any;
  formId: any;
  selectedFile!: File
  imageSrc: any;
  submitted = false;

  private unsubscribe: Subscription[] = [];
  constructor(private fb: FormBuilder,
    private route: ActivatedRoute,
    private _notifi: NotificationService,
    private brandService: BrandService
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
      this.brandFrm = this.fb.group({
        code: ['', [Validators.required, Validators.pattern('[a-zA-Z0-9._-]{3,255}')]],
        name: ['', Validators.required],
        logo: [null],
        country: [''],
        mall: [false],
        slogan: [''],
        description: ['']
      })
    }
    checkFrm(): void {
      this.formType = this.dataDialog.formType;
      if(this.formType == 'edit') {
        this.formId = this.dataDialog.id;
        this.brandFrm.controls['code'].disable();
        this.getById();
      }
    }
    //#endregion

    //#region INIT
    getById() {
      try{
        const sub = this.brandService.getById(this.formId).subscribe((res: any) => {
          if(res.status) {
            this.brand = res.data;
            this.brandFrm.patchValue(this.transformData(res.data));
            this.imageSrc =  res.data.logo ?'data:image/jpg;base64,' + res.data.logo : null;
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
          let dataForm = this.brandFrm.value;
          dataForm.status = 'ACTIVE';
          if(this.imageSrc) {
            const [, base64Data] = this.imageSrc.split(',');
            dataForm.logo = base64Data;
          }
          const postData = JSON.stringify(dataForm, null, 4);
          const sub = this.brandService.create(postData).subscribe((res: any) => {
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
          let dataForm = this.brandFrm.value;
          let data = this.transformData(dataForm);
          if(this.imageSrc) {
            const [, base64Data] = this.imageSrc.split(',');
            data.logo = base64Data;
          }
          const sub = this.brandService.update(this.formId, data).subscribe((res: any) => {
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
        id: this.brand.id,
        code: this.brand.code,
        name: data.name,
        logo: data.logo,
        country: data.country,
        slogan: data.slogan,
        mall: data.mall,
        description: data.description,
        status: this.brand.status
      }
      return oj;
    }
    //#endregion

    //#region ACTION
    onSubmit(): void {
      this.submitted = true;
      if (this.brandFrm.valid) {
        this.save();
      }
    }
    cancel(){
      this.cancelClicked.emit()
    }
    uploadFile(id: string): void {
      try {
        const sub = this.brandService.image(id, this.selectedFile).subscribe((res: any) => {
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

    //#region EVENT
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
    hasErrorInput(controlName: string, errorName: string): boolean {
      const control = this.brandFrm.get(controlName);
      if (control == null) {
        return false;
      }
      return (control.dirty || control.touched) && control.hasError(errorName);
    }
    //#endregion
}
