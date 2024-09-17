
import { Component, ElementRef, Input, OnInit, ViewChild } from '@angular/core';
import { NgbModalConfig, NgbModal, NgbActiveModal } from '@ng-bootstrap/ng-bootstrap';


@Component({ selector: 'confirm-modal-component',
  standalone: true,
  templateUrl: './confirm.modal.html',
  styleUrls: ['./confirm.modal.scss']
 })
export class ConfirmModalComponent {
  @Input() body: any;
  @Input() titleModal: any = "Cảnh báo";
  @Input() buttonSave: any = "Đồng ý";
  @Input() buttonCancel: any ="Đóng";
  @Input() className: any;
  constructor(public activeModal: NgbActiveModal) { }
}
