import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { HttpClientService } from './httpClient.service';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class OrderService {
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
    })
  };
  constructor(private http: HttpClient, private httpService: HttpClientService){}

  orderSelected: any = [];
  //#region CRUD
  search(data: any): Observable<any> {
    let httpParams = new HttpParams();
    Object.keys(data).forEach(function (key) {
      httpParams = httpParams.append(key, data[key]);
    });
    const httpOptions = {
      headers: new HttpHeaders({
        'Content-Type': 'application/json',
      }),
      params: httpParams
    };
    return this.http.get(`${environment.baseUrl}${environment.basePath}/order`, httpOptions);
  }

  create(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/order`, data);
  }

  update(id: string, data: any): Observable<any> {
    return this.http.put(`${environment.baseUrl}${environment.basePath}/order/` +  id, data, this.httpOptions);
  }

  detail(id: string): Observable<any> {
    return this.http.get(`${environment.baseUrl}${environment.basePath}/order/` + id);
  }

  addProduct(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/order/item`, data);
  }

  updateStatus(id: any, data: any) {
    return this.http.put(`${environment.baseUrl}${environment.basePath}/order/status/` +  id, data, this.httpOptions);
  }

  getByUser(id: string, data: any): Observable<any> {
    let httpParams = new HttpParams();
    Object.keys(data).forEach(function (key) {
      httpParams = httpParams.append(key, data[key]);
    });
    const httpOptions = {
      headers: new HttpHeaders({
        'Content-Type': 'application/json',
      }),
      params: httpParams
    };
    return this.http.get(`${environment.baseUrl}${environment.basePath}/order/user/` + id, httpOptions);
  }
  //#endregion
}
