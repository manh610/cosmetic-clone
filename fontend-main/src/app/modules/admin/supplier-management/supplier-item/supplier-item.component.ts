import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { AbstractControl, FormBuilder, FormGroup, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { Subscription, ReplaySubject, Subject, takeUntil } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { icons, notifi } from 'src/app/core/models/constants';
import { SupplierService } from 'src/app/core/services/supplier.service';

@Component({
  selector: 'app-supplier-item',
  templateUrl: './supplier-item.component.html',
  styleUrls: ['./supplier-item.component.scss']
})
export class SupplierItemComponent implements OnInit, OnDestroy{
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();

  supplierFrm!: FormGroup;
  supplier: any;
  formType: any;
  formId: any;
  submitted = false;

  private unsubscribe: Subscription[] = [];
  constructor(private fb: FormBuilder,
    private route: ActivatedRoute,
    private _notifi: NotificationService,
    private supplierService: SupplierService
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
    this.supplierFrm = this.fb.group({
      code: ['', [Validators.required, Validators.pattern('[a-zA-Z0-9._-]{3,255}')]],
      name: ['', Validators.required],
      email: ['', [Validators.required, Validators.email]],
      phone: ['',  Validators.compose([Validators.required, Validators.pattern('[0-9]{10,15}')])],
      country: [''],
      description: ['']
    })
  }
  checkFrm(): void {
    this.formType = this.dataDialog.formType;
    if(this.formType == 'edit') {
      this.formId = this.dataDialog.id;
      this.supplierFrm.controls['code'].disable();
      this.getById();
    }
  }
  //#endregion

  //#region INIT
  getById() {
    try{
      const sub = this.supplierService.getById(this.formId).subscribe((res: any) => {
        if(res.status) {
          this.supplier = res.data;
          this.supplierFrm.patchValue(this.transformData(res.data));
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
        let dataForm = this.supplierFrm.value;
        const sub = this.supplierService.create(dataForm).subscribe((res: any) => {
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
        let dataForm = this.supplierFrm.value;
        let data = this.transformData(dataForm);
        const sub = this.supplierService.update(this.formId, data).subscribe((res: any) => {
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
      id: this.supplier.id,
      code: this.supplier.code,
      name: data.name,
      email: data.email,
      phone: data.phone,
      country: data.country,
      description: data.description,
      status: this.supplier.status
    }
    return oj;
  }
  //#endregion

  //#region ACTION
  onSubmit(): void {
    this.submitted = true;
    if (this.supplierFrm.valid) {
      this.save();
    }
  }
  cancel(){
    this.cancelClicked.emit()
  }
  //#endregion

  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.supplierFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
}
