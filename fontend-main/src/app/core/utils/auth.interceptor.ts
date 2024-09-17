import { Injectable } from '@angular/core';
import {
  HttpEvent,
  HttpInterceptor,
  HttpHandler,
  HttpRequest
} from '@angular/common/http';
import { Observable } from 'rxjs';
import { switchMap, take } from 'rxjs/operators';
import { AuthStore } from '../stores/auth.store'; 

@Injectable()
export class AuthInterceptor implements HttpInterceptor {
  constructor(private authStore: AuthStore) {}

  intercept(req: HttpRequest<unknown>, next: HttpHandler): Observable<HttpEvent<unknown>> {
    return this.authStore.tokenReceived$.pipe(
      take(1),
      switchMap((token) => {
        if (!token) {
          return next.handle(req);
        }
        const headers = req.headers.set('Authorization', `Bearer ${token}`);
        const authReq = req.clone({
          headers
        });
        return next.handle(authReq);
      })
    );
  }
}
