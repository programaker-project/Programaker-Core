import { Component, Inject, ViewChild, ElementRef, AfterViewInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Validators, FormControl } from '@angular/forms';

declare const Huebee: any;

const UrlPattern = new RegExp(/(https?:\/\/.{2,}\..{2,})|(mailto:.*@.*)/);

export type UnderlineSettings = 'default' | 'none' | { color: string };

@Component({
    selector: 'app-configure-link-dialog',
    templateUrl: './configure-link-dialog.component.html',
    styleUrls: [
        './configure-link-dialog.component.scss',
        '../../../libs/css/material-icons.css',
    ],
})
export class ConfigureLinkDialogComponent  implements AfterViewInit {
    link = new FormControl('', [Validators.required, Validators.pattern(UrlPattern)]);
    text = new FormControl('', [Validators.required, Validators.min(1)]);
    openInTab: boolean;

    @ViewChild('underlineColorPicker') underlineColorPicker: ElementRef<HTMLInputElement>;
    private hbUnderlineColorPicker: any;
    customizeUnderline: boolean;
    underlineColor: string;
    noUnderline: boolean;


    // Initialization
    constructor(public dialogRef: MatDialogRef<ConfigureLinkDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: { link: string, text: string, openInTab: boolean, underline: UnderlineSettings }) {

        this.link.setValue(data.link);
        this.text.setValue(data.text);
        this.openInTab = data.openInTab;
        this.noUnderline = false;
        this.underlineColor = '#000';

        if ((data.underline) && (data.underline != 'default')) {
            this.customizeUnderline = true;

            if (data.underline === 'none') {
                this.noUnderline = true;
            }
            else {
                this.underlineColor = data.underline.color;
            }
        }
    }

    ngAfterViewInit(): void {
        if (this.customizeUnderline) {
            this.onUpdateUnderlineOptions();
        }
    }

    getUrlErrorMessage() {
        if (this.link.hasError('required')) {
            return 'You must enter a value';
        }

        return this.link.hasError('pattern') ? 'Not a valid link' : '';
    }

    onUpdateUnderlineOptions() {
        if (this.customizeUnderline && (!this.noUnderline)) {
            setTimeout(() => {
                // Color picker
                this.hbUnderlineColorPicker = new Huebee(this.underlineColorPicker.nativeElement, {
                    // options
                    notation: 'hex',
                    saturations: 2,
                    setText: true,
                    setBgColor: false,
                    staticOpen: true,
                });
                this.hbUnderlineColorPicker.setColor(this.underlineColor);

                this.hbUnderlineColorPicker.on('change', (color: string, _hue: any, _sat: any, _lum: any) => {
                    this.underlineColor = color;
                });
            }, 0);
        }
    }

    // Accept/cancel
    cancelChanges() {
        this.dialogRef.close({success: false});
    }

    acceptChanges() {
        let underlineSettings: UnderlineSettings = 'default';

        if (this.customizeUnderline) {
            if (this.noUnderline) {
                underlineSettings = 'none';
            }
            else {
                underlineSettings = { color: this.underlineColor };
            }
        }

        this.dialogRef.close({success: true, operation: 'set-link', value: {
            link: this.link.value,
            text: this.text.value,
            openInTab: this.openInTab,
            underline: underlineSettings
        }});
    }

    removeLink() {
        this.dialogRef.close({success: true, operation: 'remove-link'});
    }
}
