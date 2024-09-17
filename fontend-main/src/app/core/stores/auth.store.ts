import {Injectable} from '@angular/core';
import {ComponentStore, tapResponse} from '@ngrx/component-store';
import { ApiState } from '../models/common'; 
import { AccountInformation } from '../models/account';
import { AuthService } from '../services/auth.service';
import {EMPTY, Observable, Subject, timer} from 'rxjs';
import {concatMap, filter, map, switchMap, switchMapTo, takeUntil, tap, withLatestFrom} from 'rxjs/operators';
import { AccountService } from '../services/account.service';
import { LoginRequest } from '../models/auth';
import {Router} from '@angular/router';

export interface AuthState {
  tokenReceived: string;
  loggedIn: boolean;
  loginState: ApiState;
  accountInfo: AccountInformation;
}

@Injectable({
  providedIn: 'root'
})
export class AuthStore extends ComponentStore<AuthState> {
  userLogout = new Subject();
  constructor(private authService: AuthService, private accountService: AccountService, private router: Router) {
    super(<AuthState>{
      loggedIn: authService.isHasToken(),
      loginState: ApiState.PENDING
    });
  }

  readonly tokenReceived$ = this.select((state: any) => state.tokenReceived);
  readonly userLoggedIn$ = this.select((state: any) => state.loggedIn);
  readonly loginLoading$ = this.select((state: any) => state.loginState === ApiState.LOADING);
  readonly accountInfo$ = this.select((state: any) => state.accountInfo);

  readonly login = this.effect((trigger$: Observable<LoginRequest>) => trigger$.pipe(
    tap(() => this.patchState({loginState: ApiState.LOADING})),
    switchMap((request) => this.authService.requestAccessToken(request).pipe(
      tap((auth) => this.patchState({tokenReceived: auth.data.access_token})),
      concatMap((authInfo) => this.accountService.getUserInfo().pipe(
        map((accountInfo) => ({authInfo, accountInfo}))
      )),
      tapResponse((response) => {
        this.authService.setToken(response.authInfo.data.access_token, response.authInfo.data.expires_in);
        this.authService.setRefreshToken(response.authInfo.data.refresh_token, response.authInfo.data.refresh_expires_in);
        void this.router.navigate(['']);
        this.patchState({
          accountInfo: response.accountInfo.data || {},
          loggedIn: true,
          loginState: ApiState.SUCCESS
        });
      }, (err) => {
        console.error(err);
        this.patchState({loginState: ApiState.FAILED});
      }),
      concatMap(() => this.timerRefreshToken())
    ))
  ));

  readonly logout = this.effect((trigger$) => trigger$.pipe(
    tap(() => {
      this.userLogout.next('');///???
      this.authService.removeToken();
      void this.router.navigate(['/login']);
      this.patchState({loggedIn: false, loginState: ApiState.PENDING});
    })
  ));

  readonly setAutoRefreshToken = this.effect((trigger$) => trigger$.pipe(
    filter(() => this.authService.isHasToken()),
    tap(() => this.patchState({tokenReceived: this.authService.getToken()})),
    switchMapTo(this.timerRefreshToken())
  ));

  readonly loadUserIfNotExists = this.effect((trigger$) => trigger$.pipe(
    filter(() => this.authService.isHasToken()),
    withLatestFrom(this.accountInfo$),
    filter(([_, accountInfo]) => !accountInfo),
    concatMap((token) => this.accountService.getUserInfo().pipe(
      map((accountInfo) => ({token, accountInfo}))
    )),
    tapResponse((response) => this.patchState({ accountInfo: response.accountInfo.data || {} }), console.error),
  ));

  private timerRefreshToken(): Observable<any> {
    if (this.authService.isHasRefreshToken()) {
      return timer(this.authService.getFirstTimeRefreshToken(), this.authService.getPeriodRefreshToken()).pipe(
        switchMap(() => this.authService.requestRefreshToken().pipe(
          tapResponse((res) => {
            this.patchState({tokenReceived: res.data.access_token});
            this.authService.setToken(res.data.access_token, res.data.expires_in);
            this.authService.setRefreshToken(res.data.refresh_token, res.data.refresh_expires_in);
          }, console.error)
        )),
        takeUntil(this.userLogout)
      );
    } else {
      return EMPTY;
    }

  }
}
