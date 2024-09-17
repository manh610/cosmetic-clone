import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { environment } from 'src/environments/environment';
import { HttpClientService } from '../httpClient.service';

@Injectable({
  providedIn: 'root'
})
export class OptionDetailService {
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
    return this.http.get(`${environment.baseUrl}${environment.basePath}/option-detail`, httpOptions);
  }

  add(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/option-detail`, data);
  }

  update(id: string, data: any): Observable<any> {
    const jsonData = JSON.stringify(data);
    return this.http.put(`${environment.baseUrl}${environment.basePath}/option-detail/` +  id, jsonData, this.httpOptions);
  }

  remove(id: string): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/option-detail/` + id);
  }

  removeByAttribute(id: string): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/option-detail/attribute/` + id);
  }
  //#endregion
}
