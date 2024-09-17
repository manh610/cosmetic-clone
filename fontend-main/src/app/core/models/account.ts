import {Country, Provine} from './place';

export interface AccountResponse {
  code: number,
  data: AccountInformation,
  message: string,
  transactionId: string
}

export interface AccountInformation {
  id: String,
  username: String,
  email: String,
  phone: String,
  birthday: Date,
  gender: Number,
  avatar: String,
  status: Number,
  isDeleted: Number,
}
