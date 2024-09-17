import {ApiResponse} from './common';

export interface LoginRequest {
  username: string;
  password: string;
}

export interface AuthData {
  access_token: string;
  expires_in: number;
  refresh_token: string;
  refresh_expires_in: number;
}

export interface AuthResponse extends ApiResponse {
  data: AuthData;
}

export interface OtpInformation {
  remainingSeconds: number;
  totpSize: number;
  step: number;
}

export interface GenerateOtpRequest {
  username: string;
  password: string;
}

export interface GenerateOtpForgotPassRequest {
  email: string;
}

export interface GenerateOtpResponse extends ApiResponse {
  data: OtpInformation;
}

