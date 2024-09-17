import {ApiResponse} from "./common";

export interface Country {
  id: string,
  code: string,
  name: string
}

export interface CountryResponse extends ApiResponse{
  data: Country [],
}

export interface Provine {
  code: string,
  countryId: string,
  id: string,
  name: string
}
export interface ProvineResponse extends ApiResponse{
  data: Provine [],
}
