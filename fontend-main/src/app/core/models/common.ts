export enum ApiState {
  PENDING = 'pending',
  LOADING = 'loading',
  SUCCESS = 'success',
  FAILED = 'failed'
}

export interface ApiResponse {
  messageCode: number;
  code: number;
  message: string;
  transactionId: string;
}

export interface PageChangeEvent {
  first: number;
  rows: number;
  page?: number;
  pageCount?: number;
}

export const API_RESPONSE_SUCCESS_CODES = [1, 200, 3013];
