import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, Router } from '@angular/router';
import { StorageService } from './storage.service';
import { UserService } from '../user.service';
import { constants } from '../../models/constants';

@Injectable({
  providedIn: 'root'
})
export class RoleGuardService implements CanActivate {

  isloggedIn : boolean = false;
  accessToken: any;
  role: any;
  check: boolean = true;

  constructor(private storageService:StorageService,
    private router: Router,
    private fnConstants: constants,
    private userService: UserService) { }

  canActivate(route: ActivatedRouteSnapshot):boolean{
    const expectedRole = route.data['expectedRole'];
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      this.role = currentUser.roleId;
      this.isloggedIn = true;
      this.check = true;
    }
    if( !this.isloggedIn || this.role != expectedRole){
      this.router.navigate(['**']);
      this.check = false;
    }
    return this.check;
  }
}
