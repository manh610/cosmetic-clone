import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { environment } from 'src/environments/environment';
import { HttpClientService } from '../httpClient.service';

@Injectable({
  providedIn: 'root'
})
export class ProductService {
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
    return this.http.get(`${environment.baseUrl}${environment.basePath}/product`, httpOptions);
  }

  create(data: any): Observable<any> {
    data = JSON.parse(data);
    return this.http.post(`${environment.baseUrl}${environment.basePath}/product`, data);
  }

  update(id: string, data: any): Observable<any> {
    return this.http.put(`${environment.baseUrl}${environment.basePath}/product/` +  id, data, this.httpOptions);
  }

  getById(id: string): Observable<any> {
    return this.http.get(`${environment.baseUrl}${environment.basePath}/product/` + id);
  }

  delete(id: string): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/product/` + id);
  }

  deleteImage(id: any): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/product/image/` + id);
  }

  deleteMultiImage(ids: any): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/product/images`, ids);
  }

  changePhoto(id: any, file: File): Observable<any> {
    const formData: FormData = new FormData();
    formData.append('photo', file, file.name);

    return this.http.put(`${environment.baseUrl}${environment.basePath}/product/photo/` + id, formData);
  }

  changeImage(id: any, files: File[]): Observable<any> {
    const formData: FormData = new FormData();
    for (let i = 0; i < files.length; i++) {
      formData.append(`images`, files[i], files[i].name);
    }
    return this.http.put(`${environment.baseUrl}${environment.basePath}/product/images/` + id, formData);
  }
  //#endregion
}
