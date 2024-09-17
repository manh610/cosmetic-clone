import {
  HttpErrorResponse,
  HttpEvent,
  HttpHandler,
  HttpInterceptor,
  HttpRequest,
  HttpResponse
} from '@angular/common/http';
import {Injectable, NgZone} from '@angular/core';
import {Observable, throwError} from 'rxjs';
import {catchError, tap} from 'rxjs/operators';
import {API_RESPONSE_SUCCESS_CODES, ApiResponse} from '../models/common';
import {MessageService} from 'primeng/api';
import {ApiErrorResponse} from '../models/error-response';

@Injectable()
export class ErrorInterceptor implements HttpInterceptor {
  constructor(
    private ngzone: NgZone,
    private messageService: MessageService
  ) {
  }

  intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    return next.handle(req).pipe(
      tap((event: HttpEvent<any>) => {
        if (event instanceof HttpResponse) {
          const response: ApiResponse = event.body;
          if (!API_RESPONSE_SUCCESS_CODES.includes(response.code) &&!API_RESPONSE_SUCCESS_CODES.includes(response.messageCode)) {
            throw new ApiErrorResponse(`${response.code}`, response.message);
          }
        }
      }),
      catchError((error: any) => {
        switch (error.constructor) {
          case HttpErrorResponse:
            if (error.status === 401) {
              this.showErrorMessage('Xác thực thất bại', 'Bạn chưa đăng nhập hoặc Phiên đăng nhập đã hết hạn');
            }
            break;
          case ApiErrorResponse:
            this.showErrorMessage(`Lỗi`, error.message);
            break;
        }
        return throwError(error);
      })
    );
  }

  showErrorMessage(_: string, errorMessage: string): void {
    this.ngzone.run(() => {
      this.messageService.add({
        severity: 'error',
        detail: errorMessage,
        life: 5000
      });
    });
  }
}
