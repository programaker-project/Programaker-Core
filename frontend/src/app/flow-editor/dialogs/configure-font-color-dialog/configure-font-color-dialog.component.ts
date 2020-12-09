import { AfterViewInit, Component, ElementRef, Inject, ViewChild } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

declare const Huebee: any;

@Component({
    selector: 'app-configure-font-color-dialog',
    templateUrl: './configure-font-color-dialog.component.html',
    styleUrls: [
        './configure-font-color-dialog.component.scss',
        '../../../libs/css/material-icons.css',
    ],
})
export class ConfigureFontColorDialogComponent implements AfterViewInit {
    @ViewChild('textColorPicker') textColorPicker: ElementRef<HTMLInputElement>;
    @ViewChild('textSampleResult') textSampleResult: ElementRef<HTMLDivElement>;
    private hbTextColorPicker: any;

    // Initialization
    constructor(public dialogRef: MatDialogRef<ConfigureFontColorDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: { text: string, color: string }) {
    }

    ngAfterViewInit(): void {
        this.textSampleResult.nativeElement.innerText = this.data.text;

        const startingColor = this.data.color;

        // Color picker
        this.hbTextColorPicker = new Huebee(this.textColorPicker.nativeElement, {
            // options
            notation: 'hex',
            saturations: 2,
            setText: true,
            setBgColor: false,
            staticOpen: true,
        });
        this.hbTextColorPicker.setColor(startingColor);

        this.textSampleResult.nativeElement.style.color = startingColor;
        this.hbTextColorPicker.on('change', (color: string, _hue: any, _sat: any, _lum: any) => {
            this.textSampleResult.nativeElement.style.color = color;
        });
    }

    // Accept/cancel
    cancelChanges() {
        this.dialogRef.close({success: false});
    }

    acceptChanges() {
        this.dialogRef.close({success: true, operation: 'set-color', value: {
            color: this.hbTextColorPicker.color,
        }});
    }

    removeColor() {
        this.dialogRef.close({success: true, operation: 'remove-color'});
    }
}
