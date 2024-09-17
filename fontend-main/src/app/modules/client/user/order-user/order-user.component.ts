import { AfterViewInit, Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { constants, notifi } from 'src/app/core/models/constants';
import { DataSharingService } from 'src/app/core/services/data-share.service';

@Component({
  selector: 'app-order-user',
  templateUrl: './order-user.component.html',
  styleUrls: ['./order-user.component.scss']
})
export class OrderUserComponent implements OnInit, AfterViewInit{
  private unsubscribe: Subscription[] = [];

  selectedData: any = undefined;
  selectedTabIndex: number = 0;
  constructor(
    private dataShare: DataSharingService,
    ){}

    ngAfterViewInit(): void {
      this.reload();
    }
    ngOnInit(): void {

    }

    onTabChangeRequested(tabIndex: any): void {
      this.selectedTabIndex = tabIndex;
      this.initMatTabData();
    }
    onTabChange(event: any): void {
      this.dataShare.setData(event);
      this.selectedTabIndex = event;
    }
    initMatTabData() {
      const resp = this.dataShare.getData().subscribe((data: any) => {
        if (data != null) {
          this.selectedData = data;
        } else {
          this.selectedData = undefined;
        }
      });
    }

    reload() {
      this.selectedTabIndex = 0;
      this.dataShare.setData(null);
    }
}
