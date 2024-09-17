import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { HttpClientService } from './httpClient.service';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class SupplierService {
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
    return this.http.get(`${environment.baseUrl}${environment.basePath}/supplier`, httpOptions);
  }

  create(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/supplier`, data);
  }

  update(id: string, data: any): Observable<any> {
    return this.http.put(`${environment.baseUrl}${environment.basePath}/supplier/` +  id, data, this.httpOptions);
  }

  delete(id: string): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/supplier/` + id);
  }

  getById(id: string): Observable<any> {
    return this.http.get(`${environment.baseUrl}${environment.basePath}/supplier/` + id);
  }
  //#endregion
}
