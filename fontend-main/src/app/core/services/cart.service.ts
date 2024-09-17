import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { HttpClientService } from './httpClient.service';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class CartService {
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
    })
  };

  cartTotal: any;
  constructor(private http: HttpClient, private httpService: HttpClientService){}

  //#region CRUD
  search(userId: string): Observable<any> {
    return this.http.get(`${environment.baseUrl}${environment.basePath}/cart/` + userId);
  }

  addProduct(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/cart`, data);
  }

  update(id: any, quantity: any): Observable<any> {
    return this.http.put(`${environment.baseUrl}${environment.basePath}/cart/` +  id + '/' + quantity, this.httpOptions);
  }

  remove(id: string): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/cart/` + id);
  }

  deleteMulti(ids: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/cart/delete`, ids);
  }
  //#endregion
}
