import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import {environment} from '../../../environments/environment';
import { constants } from '../models/constants';
@Injectable({
  providedIn: 'root'
})
export class HttpClientService {
  httpOptions: any;
  httpOptionsUpload: any;
  // private authLocalStorageToken = `${environment.clientId}`;

  constructor(private http: HttpClient, private fnConstants: constants) { }
  // checkToken() {
  //   const auth = this.fnConstants.getFromLocalStorage(this.authLocalStorageToken);
  //   let token = '';
  //   if (!auth || !auth.accessToken) {
  //     token = auth.accessToken;
  //   }
  //   this.httpOptions = {
  //     headers: new HttpHeaders({
  //       'Content-Type': 'application/json',
  //       'Authorization': 'Bearer ' + token,
  //     })
  //   };
  // }

  // checkTokenUpload() {
  //   this.httpOptionsUpload = {
  //     headers: new HttpHeaders({
  //       'Content-Type': 'multipart/form-data',
  //     })
  //   };
  // }
  get(url: string, options?: any): Observable<any> {
    url = this.updateUrl(url);
    // this.checkToken();
    return this.http.get(url, options);
  }

  // getByParams(url: string, queryParams: HttpParams): Observable<any> {
  //   url = this.updateUrl(url);
  //   this.httpOptions = {
  //     params : queryParams
  //   };

  //   return this.http.get(url, this.httpOptions);
  // }

  // post(url: string, body: string, options?: any): Observable<any> {
  //   url = this.updateUrl(url);
  //   // this.checkToken();
  //   return this.http.post(url, body, options);
  // }

  // put(url: string, body: string, options?: any): Observable<any> {
  //   url = this.updateUrl(url);
  //   // this.checkToken();
  //   return this.http.put(url, body, options);
  // }
  // patch(url: string, body: any, options?: any): Observable<any> {
  //   url = this.updateUrl(url);
  //   return this.http.patch(url, body, options);
  // }
  // delete(url: string, options?: any): Observable<any> {
  //   url = this.updateUrl(url);
  //   // this.checkToken();
  //   return this.http.delete(url, options);
  // }

  // deleteByParams(url: string, queryParams: HttpParams): Observable<any> {
  //   url = this.updateUrl(url);
  //   this.httpOptions = {
  //     params : queryParams
  //   };

  //   return this.http.delete(url, this.httpOptions);
  // }

  // upload(url: string, body: string, options?: any): Observable<any> {
  //   url = this.updateUrl(url);
  //   this.checkTokenUpload();
  //   return this.http.post(url, body, options);
  // }

  private updateUrl(req: string) {
    return `${environment.baseUrl}${environment.basePath}` + req;
  }

}
