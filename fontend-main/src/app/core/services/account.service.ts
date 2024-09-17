import { Injectable } from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {environment} from '../../../environments/environment';
import {Observable} from 'rxjs';
import { AccountResponse } from '../models/account'; 

@Injectable({
  providedIn: 'root'
})
export class AccountService {
  constructor(private http: HttpClient) { }

  getUserInfo(): Observable<AccountResponse> {
    return this.http.get<AccountResponse>(`${environment.baseUrl}${environment.basePath}/user/current-user`);
  }
}
