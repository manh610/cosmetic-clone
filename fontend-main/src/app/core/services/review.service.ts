import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { HttpClientService } from './httpClient.service';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class ReviewService {
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
    })
  };
  constructor(private http: HttpClient, private httpService: HttpClientService){}

  reviewSelected: any = [];
  //#region CRUD
  search(id: string, data: any): Observable<any> {
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
    return this.http.get(`${environment.baseUrl}${environment.basePath}/review/` +id, httpOptions);
  }

  create(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/review`, data);
  }

  delete(id: any): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/review/` + id);
  }

  changeImage(id: any, files: File[]): Observable<any> {
    const formData: FormData = new FormData();
    for (let i = 0; i < files.length; i++) {
      formData.append(`images`, files[i], files[i].name);
    }
    return this.http.put(`${environment.baseUrl}${environment.basePath}/review/images/` + id, formData);
  }
  //#endregion
}
