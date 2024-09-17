import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, Router } from '@angular/router';
import { StorageService } from './storage.service';
import { constants } from '../../models/constants';

@Injectable({
  providedIn: 'root'
})
export class AuthGuardService implements CanActivate {

  isloggedIn : boolean = false;
  accessToken: any;
  constructor(private storageService: StorageService,
    private router: Router,
    private fnConstants: constants) { }

  // canActivate():boolean{
  //   this.accessToken = this.storageService.isLoggedIn();
  //   this.accessToken = JSON.parse(this.accessToken);
  //   if(!this.accessToken) {
  //     this.router.navigate(['**']);
  //     return false;
  //   }
  //   return true;
  // }
  canActivate():boolean{
    const currentUser = this.fnConstants.getFromLocalStorage("currentUser");
    if(currentUser) {
      return true;
    }
    return false;
  }
}
