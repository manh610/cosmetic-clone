import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { environment } from 'src/environments/environment';
import { HttpClientService } from '../httpClient.service';

@Injectable({
  providedIn: 'root'
})
export class ValueDetailService {
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
    })
  };
  constructor(private http: HttpClient, private httpService: HttpClientService){}

  create(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/value-detail`, data);
  }

  createItem(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/product-item`, data);
  }

  update(data: any): Observable<any> {
    return this.http.put(`${environment.baseUrl}${environment.basePath}/value-detail/` + data.id, data);
  }

  getById(id: any): Observable<any> {
    return this.http.get(`${environment.baseUrl}${environment.basePath}/value-detail/` + id);
  }
}
