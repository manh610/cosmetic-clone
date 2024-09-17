import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Subscription } from 'rxjs';
import { notifi } from 'src/app/core/models/constants';
import { NotificationService } from 'src/app/core/services/notification.service';
import { ValueDetailService } from 'src/app/core/services/product/value-detail.service';

@Component({
  selector: 'app-value-detail',
  templateUrl: './value-detail.component.html',
  styleUrls: ['./value-detail.component.scss']
})
export class ValueDetailComponent implements OnInit, OnDestroy{
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();

  private unsubscribe: Subscription[] = [];

  variantFrm!: FormGroup;
  formId: any;
  variant$: any;
  selectedFile!: File;
  imageSrc: any;
  submitted = false;

  constructor(private fb: FormBuilder,
    private _notifi: NotificationService,
    private variantService: ValueDetailService) {

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
    this.initVariant();
  }
  //#region FORM
  initFrm(): void {
    this.variantFrm = this.fb.group({
      importPrice: [0, [Validators.required, Validators.pattern('[0-9.]{1,255}')]],
      sellPrice: [0, [Validators.required, Validators.pattern('[0-9.]{1,255}')]],
      importQuantity: [0, [Validators.required, Validators.pattern('[0-9.]{1,255}')]],
      sellQuantity: [0, [Validators.required, Validators.pattern('[0-9.]{1,255}')]],
      status: ['', Validators.required],
      unit: [''],
      description: [''],
      image: [''],
      value: [''],
      productId: ['']
    });
  }
  checkFrm(): void {
    this.formId = this.dataDialog.id;
    this.variantFrm.controls['importPrice'].disable();
    this.variantFrm.controls['sellPrice'].disable();
    this.variantFrm.controls['unit'].disable();
  }
  //#endregion

  initVariant() {
    try{
      const sub = this.variantService.getById(this.formId).subscribe((res: any) => {
        if(res.status) {
          this.variant$  = res.data;
          this.variantFrm.patchValue(this.transformData(res.data));
          this.imageSrc =  res.data.image ?'data:image/jpg;base64,' + res.data.image : null;
        }
      }, (error: any) => {
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(ex){
      this._notifi.showError(ex, notifi.FAIL);
    }
  }

  transformData(data:any){
    let oj = {
      id: this.variant$.id,
      importPrice: this.variant$.importPrice,
      sellPrice: this.variant$.sellPrice,
      importQuantity: data.importQuantity,
      sellQuantity: data.sellQuantity,
      status: data.status,
      unit: this.variant$.unit,
      description: data.description,
      image: data.image,
      value: this.variant$.value,
      productId: this.variant$.productId
    }
    return oj;
  }

  save(): void {
    try {
      let dataForm = this.variantFrm.value;
      let data = this.transformData(dataForm);
      if(this.imageSrc) {
        const [, base64Data] = this.imageSrc.split(',');
        data.image = base64Data;
      }
      const sub = this.variantService.update(data).subscribe((res: any) => {
        if(res.status) {
          this.saveClicked.emit();
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

  //#region ACTION
  onSubmit(): void {
    this.submitted = true;
    if (this.variantFrm.valid) {
      this.save();
    }
  }
  cancel(){
    this.cancelClicked.emit()
  }
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.variantFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
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
  //#endregion
}
