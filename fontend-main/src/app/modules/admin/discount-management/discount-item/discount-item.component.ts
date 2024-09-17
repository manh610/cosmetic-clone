import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { AbstractControl, FormBuilder, FormControl, FormGroup, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { Subscription, ReplaySubject, Subject, takeUntil } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { icons, notifi } from 'src/app/core/models/constants';
import { DiscountService } from 'src/app/core/services/discount.service';

@Component({
  selector: 'app-discount-item',
  templateUrl: './discount-item.component.html',
  styleUrls: ['./discount-item.component.scss']
})
export class DiscountItemComponent implements OnInit, OnDestroy {
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();

  discountFrm!: FormGroup;
  discount: any;
  formType: any;
  formId: any;
  selectedFile!: File
  imageSrc: any;
  submitted = false;
  isSubmit: boolean = true;

  userDiscount: any = [];
  productDiscount: any = [];
  productItemDiscount: any = [];
  private unsubscribe: Subscription[] = [];
  constructor(private fb: FormBuilder,
    private route: ActivatedRoute,
    private _notifi: NotificationService,
    private discountService: DiscountService
    ){}

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
    this.discountFrm = this.fb.group({
      code: ['', [Validators.required, Validators.pattern('[a-zA-Z0-9._-]{3,255}')]],
      name: ['', Validators.required],
      startDate: new FormControl(),
      endDate: new FormControl(),
      discountType: ['', Validators.required],
      value: [1, [Validators.required, Validators.pattern('[0-9]{1,255}'), Validators.max(100)]],
      path: [''],
      show: [false],
      image: [],
      description: ['']
    })
  }
  checkFrm(): void {
    this.formType = this.dataDialog.formType;
    if(this.formType == 'edit') {
      this.formId = this.dataDialog.id;
      this.discountFrm.controls['code'].disable();
      this.getById();
    }
  }
  //#endregion

  //#region INIT
  getById() {
    try{
      const sub = this.discountService.detail(this.formId).subscribe((res: any) => {
        if(res.status) {
          this.discount = res.data;
          this.discountFrm.patchValue(this.transformData(res.data));
          this.imageSrc =  res.data.image ?'data:image/jpg;base64,' + res.data.image : null;
          if(this.discount.users) {
            this.userDiscount = this.discount.users;
            this.isSubmit = false;
            this.discountFrm.disable();
          }
          if(this.discount.products) {
            this.productDiscount = this.discount.products;
            this.isSubmit = false;
            this.discountFrm.disable();
          }
          if(this.discount.productItems) {
            this.productItemDiscount = this.discount.productItems;
            this.isSubmit = false;
            this.discountFrm.disable();
          }
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
        let dataForm = this.discountFrm.value;
        if(this.imageSrc) {
          const [, base64Data] = this.imageSrc.split(',');
          dataForm.image = base64Data;
        }
        const sub = this.discountService.create(dataForm).subscribe((res: any) => {
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
        let dataForm = this.discountFrm.value;
        let data = this.transformData(dataForm);
        if(this.imageSrc) {
          const [, base64Data] = this.imageSrc.split(',');
          data.image = base64Data;
        }
        const sub = this.discountService.update(this.formId, data).subscribe((res: any) => {
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
      id: this.discount.id,
      code: this.discount.code,
      name: data.name,
      startDate: data.startDate,
      endDate: data.endDate,
      discountType: data.discountType,
      value: data.value ?? 1,
      description: data.description,
      path: data.path,
      show: data.show,
      image: data.image
    }
    return oj;
  }
  //#endregion

  //#region ACTION
  onSubmit(): void {
    this.submitted = true;
    if (this.discountFrm.valid) {
      this.save();
    }
  }
  cancel(){
    this.cancelClicked.emit()
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
    const control = this.discountFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  //#endregion
}
