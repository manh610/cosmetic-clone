import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { HttpClientService } from './httpClient.service';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class AddressService {
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
    })
  };
  constructor(private http: HttpClient, private httpService: HttpClientService){}

  //#region CRUD
  getProvinces(data: any): Observable<any> {
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
    return this.http.get(`${environment.baseUrl}${environment.basePath}/address/provinces`, httpOptions);
  }
  getDistricts(data: any): Observable<any> {
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
    return this.http.get(`${environment.baseUrl}${environment.basePath}/address/districts`, httpOptions);
  }

  getWard(data: any): Observable<any> {
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
    return this.http.get(`${environment.baseUrl}${environment.basePath}/address/wards`, httpOptions);
  }

  addUser(data: any): Observable<any> {
    data = JSON.parse(data);
    return this.http.post(`${environment.baseUrl}${environment.basePath}/address/user`, data);
  }

  updateUser(data: any, id: any): Observable<any> {
    return this.http.put(`${environment.baseUrl}${environment.basePath}/address/user/` +  id, data, this.httpOptions);
  }

  delete(id: any): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/address/` + id);
  }

  changeDefault(userId: any, addressId: any): Observable<any> {
    return this.http.put(`${environment.baseUrl}${environment.basePath}/address/` + userId + `/` + addressId, '');
  }
  //#endregion
}
