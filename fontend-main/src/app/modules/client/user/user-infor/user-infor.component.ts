import { AfterViewInit, Component } from '@angular/core';
import { DataSharingService } from 'src/app/core/services/data-share.service';

@Component({
  selector: 'app-user-infor',
  templateUrl: './user-infor.component.html',
  styleUrls: ['./user-infor.component.scss']
})
export class UserInforComponent implements AfterViewInit  {
  constructor(private dataShare: DataSharingService,){}
  ngAfterViewInit(): void {
  }
  selectedData:any = undefined;
  selectedTabIndex: number = 0;
  onTabChangeRequested(tabIndex: any): void {
    this.selectedTabIndex = tabIndex;
    this.initMatTabData();
  }
  onTabChange(event: any): void {
    this.selectedTabIndex = event;
  }
  initMatTabData() {
    const resp = this.dataShare.getData().subscribe(data => {
      if (data != null) {
        this.selectedData = data;
      }else{
        this.selectedData = undefined;
      }
    });
  }
  reload(){
    this.selectedTabIndex = 0;
    this.dataShare.setData(null);
  }
}
