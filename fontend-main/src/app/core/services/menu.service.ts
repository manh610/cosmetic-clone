import { Injectable } from '@angular/core';
import {environment} from '../../../environments/environment';
import { Observable } from 'rxjs';
import { HttpParams } from '@angular/common/http';
import { constants, factory } from '../models/constants';
import { MenuItem } from '../models/menu';
import { HttpClientService } from './httpClient.service';

@Injectable({
  providedIn: 'root'
})
export class MenuService {
  // private authLocalStorageToken = `${environment.clientId}`;

  constructor(private httpClient: HttpClientService, private fnConstants: constants) { }

  // getMenuNavByPermission(): any{
  //   let menu: MenuItem[] = [];
  //   try{
  //     return this.httpClient.get(factory.API_MENU + '/menu-for-user');
  //   }
  //   catch(error) {
  //     console.error(error);
  //     return menu;
  //   }
  // }

  // getMenuTreeView(): Observable<any>{
  //   return this.httpClient.get(factory.API_MENU);
  // }

  // search(data: any): Observable<any> {
  //   let httpParams = new HttpParams();
  //   Object.keys(data).forEach(function (key) {
  //     httpParams = httpParams.append(key, data[key]);
  //   });

  //   return this.httpClient.getByParams(factory.API_MENU + '/search', httpParams);
  // }

  // getById(id: string): Observable<any>{
  //   return this.httpClient.get(factory.API_MENU + '/' + id);
  // }

  // update(request: any): Observable<any> {
  //   return this.httpClient.put(factory.API_MENU + '/' + request.id, request);
  // }

  // create(request: any): Observable<any> {
  //   return this.httpClient.post(factory.API_MENU + '/', request);
  // }

  // delete(id: string): Observable<any> {
  //   return this.httpClient.delete(factory.API_MENU + '/' + id);
  // }

  // getChildrenById(id: string, data: any): Observable<any>{
  //   let httpParams = new HttpParams();
  //   Object.keys(data).forEach(function (key) {
  //     httpParams = httpParams.append(key, data[key]);
  //   });
  //   return this.httpClient.getByParams(factory.API_MENU + '/' + id + '/children', httpParams);
  // }
  // getMenuTreeView(): Observable<any> {
  //   return this.httpClient.get(factory.API_MENU+'/');
  // }


}
