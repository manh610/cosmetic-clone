export class ApiErrorResponse extends Error {
  constructor(public code: string, message: string) {
    super(message);
    Object.setPrototypeOf(this, ApiErrorResponse.prototype);
  }
}

export class ApiErrorTokenInvalid extends Error {
  constructor(public code: string, message: string) {
    super(message);
    Object.setPrototypeOf(this, ApiErrorTokenInvalid.prototype);
  }
}
