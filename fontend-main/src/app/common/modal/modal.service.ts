import {
  Injectable,
} from '@angular/core';
import { NgbActiveModal, NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { ConfirmModalComponent } from './confirm/confirm.modal';
@Injectable({ providedIn: 'root' })
export class ModalService {


  constructor(private modalService: NgbModal) { }

  public openDialogConfirm(title?: string, body?: any,btnSave?: any, btnCancel?: any, option?: any ) {
    const modalRef = this.modalService.open(ConfirmModalComponent, option);//option dưới dạng json style VD: { size: 'xl' } cho modal to hơn
    modalRef.componentInstance.titleModal = title;
    modalRef.componentInstance.body = body;
    modalRef.componentInstance.buttonSave = btnSave;
    modalRef.componentInstance.buttonCancel = btnCancel;
    return modalRef;
  }
  public openDialogTemplate(modalComponent: any, option?:any) {
    const modalRef = this.modalService.open(modalComponent, option);
    return modalRef;
  }
}
