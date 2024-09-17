import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, map, of } from 'rxjs';
import { HttpClientService } from './httpClient.service';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class FavoriteService {
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
    })
  };

  favoriteTotal: any;
  constructor(private http: HttpClient, private httpService: HttpClientService){}

  //#region CRUD
  search(userId: string): Observable<any> {
    return this.http.get(`${environment.baseUrl}${environment.basePath}/favorite/` + userId);
  }

  addProduct(data: any): Observable<any> {
    return this.http.post(`${environment.baseUrl}${environment.basePath}/favorite`, data);
  }
  removeProduct(userId: string, productId: string): Observable<any> {
    return this.http.delete(`${environment.baseUrl}${environment.basePath}/favorite/` + userId + `/` + productId);
  }
  //#endregion
}
