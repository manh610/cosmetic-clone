import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class DataSharingService {
  private dataSubject = new BehaviorSubject<any>(null);

  setData(data: any): void {
    this.dataSubject.next(data);
  }
  getData(){
    return this.dataSubject.asObservable()
  }
  removeData() {
    this.dataSubject.unsubscribe();
  }
}
