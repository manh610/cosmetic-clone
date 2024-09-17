
import { Injectable } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import domtoimage from 'dom-to-image';
import { TranslationService } from 'src/app/modules/i18n';
import { NotificationService } from './notification.service';

@Injectable({
  providedIn: 'root',
})
export class ExportChartService {
  constructor(private translate: TranslateService,
    private translation: TranslationService,
    private notifi: NotificationService){}
  exportToImage(elementId: any, width: any, height: any): void {
    const node = document.getElementById(elementId);

    if (!node) {
      this.notifi.showInfo(`Element with ID '${elementId}' not found.`, this.translation.localize("NOTIFI.INFOR", null));
      return;
    }

    setTimeout(() => {
      domtoimage.toPng(node, {
        width: width,
        height: height,
        bgcolor: '#ffffff'
      })
      .then( (dataUrl) => {
        if(dataUrl != 'data:,') {
          const link = document.createElement('a');
          link.href = dataUrl;
          link.download = 'chart.png';
          link.click();
        } else {
          this.notifi.showError('Error: Data URL is null.', this.translation.localize("NOTIFI.ERROR", null));
        }
      })
      .catch((error) => {
        console.error('Error exporting to image:', error);
      });
    }, 100); // Adjust the timeout value as needed
  }
}
