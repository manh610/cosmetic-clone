import { Component, ElementRef, Input, OnInit, ViewChild } from '@angular/core';
import { NgbModalConfig, NgbModal } from '@ng-bootstrap/ng-bootstrap';

@Component({
  selector: 'ngbd-modal-config',
  standalone: true,
  templateUrl: './modal-config.html',
  styleUrls: ['./modal-config.scss'],
  // add NgbModalConfig and NgbModal to the component providers
  providers: [NgbModalConfig, NgbModal],
})
export class NgbdModalConfig implements OnInit {
  @Input() id!: string;
  private element: any;

  constructor(config: NgbModalConfig, private modalService: NgbModal, private el: ElementRef) {
    // customize default values of modals used by this component tree
    config.backdrop = 'static';
    config.keyboard = false;
  }
  ngOnInit(): void {
  }
  open(content:any) {
    this.modalService.open(content);
  }

}
