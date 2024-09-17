import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-flip-clock',
  templateUrl: './flip-clock.component.html',
  styleUrls: ['./flip-clock.component.scss']
})
export class FlipClockComponent implements OnInit{
  currentHour: number = 0;
  currentMinute: number = 0;
  currentSecond: number = 0;
  countdownTotalSeconds!: number;
  timePoints = [0, 9, 12, 15, 19, 21];
  constructor() {}

  ngOnInit(): void {
    const currentTime = new Date();
    const currentHour = currentTime.getHours();
    let nextTime = null;
    for (const time of this.timePoints) {
      if (time > currentHour) {
        nextTime = time;
        break;
      }
    }

    if (nextTime === null) {
      nextTime = this.timePoints[0] + 24;
    }

    const nextTimeDate = new Date(currentTime);
    nextTimeDate.setHours(nextTime, 0, 0, 0);
    this.countdownTotalSeconds = Math.floor((nextTimeDate.getTime() - currentTime.getTime()) / 1000);
    this.startCountdown();
  }

  startCountdown() {
    this.updateClockDisplay();

    setInterval(() => {
      if (this.countdownTotalSeconds > 0) {
        this.countdownTotalSeconds--;
        this.updateClockDisplay();
      }
    }, 1000);
  }

  updateClockDisplay() {
    this.currentHour = Math.floor(this.countdownTotalSeconds / 3600);
    this.currentMinute = Math.floor((this.countdownTotalSeconds % 3600) / 60);
    this.currentSecond = this.countdownTotalSeconds % 60;
  }
}
