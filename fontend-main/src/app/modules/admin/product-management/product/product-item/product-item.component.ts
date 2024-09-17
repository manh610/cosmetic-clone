import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { AbstractControl, FormArray, FormBuilder, FormGroup, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription, ReplaySubject, Subject, takeUntil } from 'rxjs';
import { NotificationService } from 'src/app/core/services/notification.service';
import { FormControl } from '@angular/forms';
import { CategoryService } from 'src/app/core/services/category.service';
import { icons, notifi, routerNav } from 'src/app/core/models/constants';
import { ProductService } from 'src/app/core/services/product/product.service';
import { BrandService } from 'src/app/core/services/brand.service';
import { SkinTypeService } from 'src/app/core/services/skin-type.service';
import { formatDate } from '@angular/common';
import { Location } from '@angular/common';
import { AttributeService } from 'src/app/core/services/product/option-attribute.service';
import { ModalService } from 'src/app/common/modal/modal.service';
import { ValueDetailService } from 'src/app/core/services/product/value-detail.service';
import { Editor, Toolbar } from 'ngx-editor';
import { ValueDetailComponent } from '../value-detail/value-detail.component';

@Component({
  selector: 'app-product-item',
  templateUrl: './product-item.component.html',
  styleUrls: ['./product-item.component.scss']
})
export class ProductItemComponent implements OnInit, OnDestroy {

  //#region VARIABLE
  productFrm!: FormGroup;
  variantFrm!: FormGroup;
  dynamicFrm!: FormGroup;
  submitted = false;
  formType: any;
  formId: any;

  product$: any;
  brand$: any;
  skinType$: any;
  category$: any;

  skinTypes: any = [];
  isVariant: boolean = false;
  editorDesc!: Editor;
  variant$: any;

  selectedFile!: File;
  delImage: any[] = [];
  imageByte: any[] = [];
  imageSrcPr: any;


  toolbar: Toolbar = [
    ['bold', 'italic', 'underline'],
    ['strike'],
    ['code', 'blockquote'],
    ['ordered_list', 'bullet_list'],
    [{ heading: ['h1', 'h2', 'h3', 'h4', 'h5', 'h6'] }],
    ['link'],
    ['text_color', 'background_color'],
    ['align_left', 'align_center', 'align_right', 'align_justify', 'horizontal_rule', 'format_clear'],
  ];
  //#endregion

  private unsubscribe: Subscription[] = [];
  constructor(private fb: FormBuilder,
    private _notifi: NotificationService,
    private _route: ActivatedRoute,
    private router: Router,
    private _location: Location,
    private modalService: ModalService,
    private notifi: NotificationService,
    private productService: ProductService,
    private brandService: BrandService,
    private skinTypeService: SkinTypeService,
    private categoryService: CategoryService,
    private attributeService: AttributeService,
    private valueService: ValueDetailService
  ) {
    this.dynamicFrm = this.fb.group({
      dynamicVariant: this.fb.array([])
    });
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
    this.editorDesc = new Editor();
    this.productFrm = this.fb.group({
      code: ['', [Validators.required, Validators.pattern('[a-zA-Z0-9._-]{3,255}')]],
      name: ['', Validators.required],
      status: ['', Validators.required],
      description: [''],
      photo: [''],
      categoryId: ['', Validators.required],
      brandId: ['', Validators.required],
      madeIn: ['', Validators.required],
      productionDate: new FormControl(),
      expirationDate: new FormControl(),
      skinTypeId: ['', Validators.required],
    });
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
    this.formType = this._route.snapshot.paramMap.get('type');
    this.initBrand();
    this.initCategory();
    this.initSkinType();
    if(this.formType == 'edit') {
      this.formId = this._route.snapshot.paramMap.get('id');
      this.productFrm.controls['code'].disable();
      this.getById();
    }
  }
  //#endregion

  //#region INIT
  initBrand() {
    try{
      const data: any = {
        keyword: '',
        status: 'ACTIVE'
      }
      const sub = this.brandService.search(data).subscribe((res: any) => {
        if(res.status) {
          this.brand$ = res.data;
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
  initSkinType() {
    try{
      const sub = this.skinTypeService.search().subscribe((res: any) => {
        if(res.status) {
          this.skinType$ = res.data;
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
  initCategory() {
    try {
      const data: any = {
        keyword: '',
        status: 'ACTIVE',
        pageIndex: 1,
        pageSize: 30
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
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  getById() {
    try{
      const sub = this.productService.getById(this.formId).subscribe((res: any) => {
        if(res.status) {
          this.product$ = res.data;
          res.data.skinTypes.forEach((x: any) => {
            this.skinTypes.push(x.id)
          })
          this.productFrm.patchValue(this.transformData(res.data));
          this.variant$ = this.product$.valueDetails;
          this.imageByte = this.product$.images;
          this.imageSrcPr =  res.data.photo ?'data:image/jpg;base64,' + res.data.photo : null;
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
        const postData = this.convertData();
        const sub = this.productService.create(postData).subscribe((res: any) => {
          if (res.status) {
            if(!this.isVariant)
              this.saveValue(res.data.id);
            if(this.isVariant)
              this.saveDyanamicValue(res.data.id);
              if(this.images.length > 0) {
                this.changeImage(res.data.id);
              }
          } else {
            this.notifi.showError('Thêm mới sản phẩm thất bại', 'Thất bại');
          }
        }, (error: any) => {
          for (let e of error.error.errors) {
            this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
          }
        })
        this.unsubscribe.push(sub);
      }else {
        let dataForm = this.productFrm.value;
        const postData = this.transformData(dataForm);
        postData.skinTypeId = dataForm.skinTypeId;
        if(this.imageSrcPr) {
          const [, base64Data] = this.imageSrcPr.split(',');
          postData.photo = base64Data;
        }
        const sub = this.productService.update(this.formId, postData).subscribe((res: any) => {
          if(res.status) {
            if(this.images.length > 0) {
              this.changeImage(res.data.id);
            }
            if(this.delImage.length > 0) {
              this.deleteMultiImage();
            }
            this.imageByte
            this.notifi.showSuccess('Cập nhật sản phẩm '+res.data.code+' thành công', 'Thành công');
            this.router.navigate([routerNav.NAV_PRODUCT]);
          }else {
            this.notifi.showError('Cập nhật sản phẩm '+res.data.code+' thất bại', 'Thất bại');
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
      id: this.product$.id,
      code: this.product$.code,
      name: data.name,
      status: data.status,
      description: data.description,
      photo: data.photo,
      categoryId: data.categoryId,
      brandId: data.brandId,
      madeIn: data.madeIn,
      productionDate: data.productionDate,
      expirationDate: data.expirationDate,
      skinTypeId: this.skinTypes,
    }
    return oj;
  }
  convertData() {
    const frmData = JSON.stringify(this.productFrm.value, null, 4);
    try{
      const productionDate  = this.productFrm.controls['productionDate'].value;
      const expirationDate = this.productFrm.controls['expirationDate'].value;

      var formatedproductionDate = new Date(productionDate).toLocaleString();
      var formatedexpirationDate = new Date(expirationDate).toLocaleString();

      this.productFrm.controls['productionDate'].setValue(formatDate(formatedproductionDate, 'yyyy-MM-ddTHH:mm', 'en-US'));
      this.productFrm.controls['expirationDate'].setValue(formatDate(formatedexpirationDate, 'yyyy-MM-ddTHH:mm', 'en-US'));

      if(this.imageSrcPr) {
        const [, base64Data] = this.imageSrcPr.split(',');
        this.productFrm.controls['photo'].setValue(base64Data);
      }

      const frmData = JSON.stringify(this.productFrm.value, null, 4);
      return frmData;
    }catch(ex){
      return frmData;
    }
  }
  saveValue(productId: any) {
    try{
      if(this.formType == 'add') {
        let dataForm = this.variantFrm.value;
        dataForm.productId = productId;
        const sub = this.valueService.create(dataForm).subscribe((res: any) => {
          if(res.status) {
            this.notifi.showSuccess('Thêm mới sản phẳm thành công', 'Thành công');
            this.router.navigate([routerNav.NAV_PRODUCT]);
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
  saveDyanamicValue(productId: any) {
    try{
      if(this.formType == 'add') {
        const arr: any = this.dynamicFrm.value.dynamicVariant;
        let cnt = 0;
        arr.forEach((x: any) => {
          x.productId = productId;
          cnt += 1;
          const sub = this.valueService.create(x).subscribe((res: any) => {
            if(res.status) {
              if(cnt == arr.length) {
                this.notifi.showSuccess('Thêm mới sản phẳm thành công', 'Thành công');
                this.router.navigate([routerNav.NAV_PRODUCT]);
              }
            }
          }, (error: any) => {
            for (let e of error.error.errors) {
              this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
            }
          })
          this.unsubscribe.push(sub);
        })
      }
    }catch(ex) {
      this._notifi.showError(ex, notifi.FAIL);
    }
  }
  changeImage(id: any) {
    try{
      const sub = this.productService.changeImage(id, this.images).subscribe((res: any) => {
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
  deleteMultiImage() {
    const data: any = {
      ids: this.delImage
    }
    try{
      const sub = this.productService.deleteMultiImage(data).subscribe((res: any) => {

      }, (error: any) =>{
        for (let e of error.error.errors) {
          this._notifi.showInfo(e.code + '-' + e.message.vn, notifi.INFO);
        }
      })
      this.unsubscribe.push(sub);
    }catch(e) {
      this._notifi.showError(e, notifi.FAIL);
    }
  }
  //#endregion

  //#region ACTION
  onSubmit(): void {
    this.submitted = true;
    if(this.formType == 'add') {
      if((this.productFrm.valid && this.variantFrm.valid) ||
        (this.productFrm.valid && this.dynamicFrm.valid)) {
          this.save();
      }
    }
    if(this.formType == 'edit' && this.productFrm.valid) {
      this.save();
    }
  }
  back() {
    this._location.back();
  }
  dialogValue(id: any) {
    let data = {
      id: id
    }
    const modalRef = this.modalService.openDialogTemplate(ValueDetailComponent, {
      size: 'lg',
    })
    modalRef.componentInstance.dataDialog = data;
    try{
      modalRef.componentInstance.saveClicked.subscribe(()  => {
        this.getById();
        this._notifi.showSuccess('Cập nhật biến thể thành công', notifi.SUCCESS);
        modalRef.close();
      }
      );
      modalRef.componentInstance.cancelClicked.subscribe(() =>{
        modalRef.close();
      })
    }catch(e) {
        this._notifi.showInfo(e, notifi.FAIL);
      }
  }
  removeImage(dataImge: any) {
    this.delImage.push(dataImge.id);
    const index = this.imageByte.slice().reverse().findIndex((item: any) => item.id === dataImge.id);
    if (index > -1 ) {
      const actualIndex = this.imageByte.length - 1 - index;
      this.imageByte.splice(actualIndex, 1);
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
          this.imageSrcPr = e.target?.result;
        };
        reader.readAsDataURL(this.selectedFile!);
      } else {
        this._notifi.showInfo('Định dạng File không hợp lệ', notifi.INFO);
      }
    }
  }
  //#endregion

  //#region EVENT
  onCheckBoxChange(event: any) {
    if(event.checked) this.isVariant = true;
    else this.isVariant = false;
  }
  // onDropdownChange(event: any, index: any) {
  //   if(event.value) {
  //     this.getByIdAttribute(event.value);
  //   } else
  //     this.value$ = null;
  // }
  hasErrorInput(controlName: string, errorName: string): boolean {
    const control = this.productFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  hasErrorInputVariant(controlName: string, errorName: string): boolean {
    const control = this.variantFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  hasErrorInputDynamicVariant(controlName: string, errorName: string): boolean {
    const control = this.dynamicFrm.get(controlName);
    if (control == null) {
      return false;
    }
    return (control.dirty || control.touched) && control.hasError(errorName);
  }
  //#endregion

  //#region FORM ARRAY
  dynamicVariant(): FormArray {
    return this.dynamicFrm.get('dynamicVariant') as FormArray;
  }

  newDynamicVariant(): FormGroup {
    return this.fb.group({
      importPrice: [0, [Validators.required, Validators.pattern('[0-9.]{1,255}')]],
      sellPrice: [0, [Validators.required, Validators.pattern('[0-9.]{1,255}')]],
      importQuantity: [0, [Validators.required, Validators.pattern('[0-9.]{1,255}')]],
      sellQuantity: [0, [Validators.required, Validators.pattern('[0-9.]{1,255}')]],
      status: ['', Validators.required],
      unit: [''],
      description: [''],
      image: [''],
      value: ['', Validators.required],
      productId: ['']
    });
  }

  addDynamicVariant() {
    this.dynamicVariant().push(this.newDynamicVariant());
  }

  removeDynamicVariant(index: number) {
    this.dynamicVariant().removeAt(index);
  }
  //#endregion

  //#region FILE
  uploadedFiles: any[] = [];
  images: any[] = [];
  onAddImage(event: any) {
    let files = event.currentFiles;
    this.images.push(event.currentFiles[files.length - 1]);
  }
  onRemoveImage(event: any) {
    const index = this.images.slice().reverse().findIndex((item: any) => item.name=== event.file.name);
    if (index > -1 ) {
      const actualIndex = this.images.length - 1 - index;
      this.images.splice(actualIndex, 1);
    }
  }
  //#endregion
}
