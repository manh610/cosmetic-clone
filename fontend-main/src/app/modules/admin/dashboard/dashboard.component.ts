import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit, OnDestroy {
  private unsubscribe: Subscription[] = [];

  view: any = [600, 400];
  viewPie: any = [500, 250];

  //#region CHART
  showXAxis = true;
  showYAxis = true;
  gradient = false;
  showLegend = true;
  showLabels = true;
  showXAxisLabel = false;
  showYAxisLabel = true;
  xAxisLabel1 = 'Sản phẩm';
  yAxisLabel1 = 'Số lượng';


  colorScheme: any = {
    domain: ['#2a73dc', '#87CEFA', '#FA8072', '#c5ce4d', '#90EE90']
  };
  //#endregion

  statProduct: any = [
    {name: 'AA', value: 200},
    {name: 'BB', value: 500},
    {name: 'CC', value: 800},
    {name: 'DD', value: 10},
    {name: 'EE', value: 80},
    {name: 'FF', value: 660},
    {name: 'GG', value: 700},
    {name: 'HH', value: 200},
    {name: 'II', value: 450},
  ]

  ngOnDestroy(): void {
    this.unsubscribe.forEach((sb) => sb.unsubscribe());
  }
  ngOnInit(): void {
  }

  //#region labelFormatting
  getTotalValue(data: any): number {
    let total = 0;
    data.forEach((slice: any) => {
      total = total + slice.value;
    });
    return total;
  }

  fortmatLabel1 = (labelname: any) => {
    const totalValue = this.getTotalValue(this.statProduct);
    const datapoint = this.statProduct.find((x: any) => x.name === labelname);
    const degrees = (datapoint.value / totalValue) * 100;
    return `${degrees.toFixed(2)}%`;
  }
  //#endregion
}
