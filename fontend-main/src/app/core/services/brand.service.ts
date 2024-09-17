import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { HttpClientService } from './httpClient.service';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class BrandService {
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
    })
  };
  constructor(private http: HttpClient, private httpService: HttpClientService){}

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
    return this.http.get(`${environment.baseUrl}${environment.basePath}/brand`, httpOptions);
  }

  create(data: any): Observable<any> {
    data = JSON.parse(data);
    return this.http.post(`${environment.baseUrl}${environment.basePath}/brand`, data);
  }

  update(id: string, data: any): Observable<any> {
    const jsonData = JSON.stringify(data);
    return this.http.put(`${environment.baseUrl}${environment.basePath}/brand/` +  id, jsonData, this.httpOptions);
  }

  getById(id: string): Observable<any> {
    return this.http.get(`${environment.baseUrl}${environment.basePath}/brand/` + id);
  }

  delete(id: string): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/brand/` + id);
  }

  image(id: string, file: File): Observable<any> {
    const formData = new FormData();
    formData.append('logo', file);
    return this.http.put(`${environment.baseUrl}${environment.basePath}/brand/logo/` + id, formData);
  }
  //#endregion
}
