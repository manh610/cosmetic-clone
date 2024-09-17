import { Component, EventEmitter, HostBinding, Input, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { animate, state, style, transition, trigger } from '@angular/animations';
import { MenuItem } from 'src/app/core/models/menu';

@Component({
  selector: 'app-menu-item',
  templateUrl: './menu-item.component.html',
  styleUrls: ['./menu-item.component.scss'],
  animations: [
    trigger('indicatorRotate', [
      state('collapsed', style({ transform: 'rotate(0deg)' })),
      state('expanded', style({ transform: 'rotate(180deg)' })),
      transition('expanded <=> collapsed',
        animate('225ms cubic-bezier(0.4,0.0,0.2,1)')
      ),
    ])
  ]
})
export class MenuItemComponent {
  //khai báo biến
  expanded: boolean = false;
  public drawer: any;
  //Biến dùng chung cho parrent component
  @Input() opened: boolean = false;
  @Input() data ="open";
  @Input() item: any;
  @Input() depth: number = 1;
  //Fix doom class
  @HostBinding('attr.aria-expanded') ariaExpanded = this.expanded;

  constructor(public router: Router) {
    if (this.depth === undefined) {
      this.depth = 0;
    }
  }

  //event
  onItemSelected(item: MenuItem) {
    if (!item.children || !item.children.length) {
      this.router.navigate([item.path]);
      this.closeNav();
    }
    if (item.children && item.children.length) {
      this.expanded = !this.expanded;
    }
  }
  closeNav() {
    if (this.drawer){
      this.drawer.close();
    }
  }

  openNav() {
    if (this.drawer){
      this.drawer.open();
    }
  }
}
