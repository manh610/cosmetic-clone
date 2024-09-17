import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { AbstractControl, FormBuilder, FormGroup, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { Subscription, ReplaySubject, Subject, takeUntil } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { FormControl } from '@angular/forms';
import { MatSelect } from '@angular/material/select';
import { TranslationService } from 'src/app/modules/i18n';
import { CategoryService } from 'src/app/core/services/category.service';
import { DropdownComponent } from 'src/app/common/dropdown/dropdown.component';
import { icons, notifi } from 'src/app/core/models/constants';

@Component({
  selector: 'app-category-item',
  templateUrl: './category-item.component.html',
  styleUrls: ['./category-item.component.scss']
})
export class CategoryItemComponent implements OnInit, OnDestroy {
  @Input() dataDialog: any;
  @Output() cancelClicked = new EventEmitter<void>();
  @Output() saveClicked = new EventEmitter<any>();

  //#region VARIABLE
  categoryFrm!: FormGroup
  category$: any;
  category: any;
  page: number = 1;
  pageSize: number = 30;
  keyword: string = '';

  icons: any;
  submitted = false;
  formType!: string;
  formId!: string;
  selectedFile!: File
  imageSrc: any;
  //#endregion

  private unsubscribe: Subscription[] = [];
  constructor(private fb: FormBuilder,
    private route: ActivatedRoute,
    private _notifi: NotificationService,
    private translation: TranslationService,
    private categoryService: CategoryService,
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
      this.initCategory();
    }

    //#region FORM
    initFrm(): void {
      this.categoryFrm = this.fb.group({
        code: ['', [Validators.required, Validators.pattern('[a-zA-Z0-9._-]{3,255}')]],
        name: ['', Validators.required],
        path: ['', Validators.required],
        image: [null],
        parentId: [''],
        icon: [''],
        offset: [ 1, [Validators.required, Validators.pattern('[0-9]{1,255}')]],
        description: ['']
      });
      this.icons = Object.entries(icons).map(([name, value]) => ({ name, value }));
    }
    checkFrm(): void {
      this.formType = this.dataDialog.formType;
      if(this.formType == 'edit') {
        this.formId = this.dataDialog.id;
        this.categoryFrm.controls['code'].disable();
        this.getById();
      }
    }
    //#endregion


    //#region INIT
    initCategory() {
      try{
        let data: any = {
          keyword: this.keyword,
          pageIndex: this.page,
          pageSize: this.pageSize,
          status: 'ACTIVE',
          parentId: ''
        }
        const sub = this.categoryService.search(data).subscribe((res: any) => {
          if(res.status) {
            this.category$ = res.data;
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
    getById() {
      try{
        const sub = this.categoryService.getById(this.formId).subscribe((res: any) => {
          if(res.status) {
            this.category = res.data;
            this.categoryFrm.patchValue(this.transformData(res.data));
            this.imageSrc =  res.data.image ?'data:image/jpg;base64,' + res.data.image : null;
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
          let dataForm = this.categoryFrm.value;
          dataForm.status = 'ACTIVE';
          if(this.imageSrc) {
            const [, base64Data] = this.imageSrc.split(',');
            dataForm.image = base64Data;
          }
          const postData = JSON.stringify(dataForm, null, 4);
          const sub = this.categoryService.create(postData).subscribe((res: any) => {
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
          let dataForm = this.categoryFrm.value;
          let data = this.transformData(dataForm);
          if(this.imageSrc) {
            const [, base64Data] = this.imageSrc.split(',');
            data.image = base64Data;
          }
          const sub = this.categoryService.update(this.formId, data).subscribe((res: any) => {
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
        id: this.category.id,
        code: this.category.code,
        name: data.name,
        path: data.path,
        image: data.image,
        parentId: data.parentId == null ? '' : data.parentId,
        icon: data.icon,
        offset: data.offset,
        description: data.description,
        status: this.category.status
      }
      return oj;
    }
    //#endregion

    //#region ACTION
    onSubmit(): void {
      this.submitted = true;
      if (this.categoryFrm.valid) {
        this.save();
      }
    }
    cancel(){
      this.cancelClicked.emit()
    }
    uploadFile(id: string): void {
      try {
        const sub = this.categoryService.image(id, this.selectedFile).subscribe((res: any) => {
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
      const control = this.categoryFrm.get(controlName);
      if (control == null) {
        return false;
      }
      return (control.dirty || control.touched) && control.hasError(errorName);
    }
    //#endregion
}
