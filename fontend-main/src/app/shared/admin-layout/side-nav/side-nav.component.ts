import { Component, ViewChild, inject } from '@angular/core';
import { BreakpointObserver, Breakpoints } from '@angular/cdk/layout';
import { Observable } from 'rxjs';
import { map, shareReplay } from 'rxjs/operators';
import { MatSidenav } from '@angular/material/sidenav';

@Component({
  selector: 'app-side-nav',
  templateUrl: './side-nav.component.html',
  styleUrls: ['./side-nav.component.scss']
})
export class SideNavComponent {
  private breakpointObserver = inject(BreakpointObserver);
  opened: string = "open";
  @ViewChild(MatSidenav)// truy cập vào child component của matsidenav angular
  sidenav!: MatSidenav;
  constructor() {

  }
  divScroll(e:any) {
    // console.log(window.scrollY, e);
  }
  isHandset$: Observable<boolean> = this.breakpointObserver.observe(Breakpoints.Handset)
    .pipe(
      map(result => result.matches),
      shareReplay()
    );
  toggleSideNav() {
    this.sidenav.toggle().then((sidenavIsOpen: string) => {
      this.opened = sidenavIsOpen.toLowerCase();
    })
  }
}
