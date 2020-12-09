import { Component, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Validators, FormControl } from '@angular/forms';

const UrlPattern = new RegExp(/(https?:\/\/.{2,}\..{2,})|(mailto:.*@.*)/);

@Component({
    selector: 'app-configure-link-dialog',
    templateUrl: './configure-link-dialog.component.html',
    styleUrls: [
        './configure-link-dialog.component.scss',
        '../../../libs/css/material-icons.css',
    ],
})
export class ConfigureLinkDialogComponent {
    link = new FormControl('', [Validators.required, Validators.pattern(UrlPattern)]);
    text = new FormControl('', [Validators.required, Validators.min(1)]);
    openInTab: boolean;

    // Initialization
    constructor(public dialogRef: MatDialogRef<ConfigureLinkDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: { link: string, text: string, openInTab: boolean }) {

        this.link.setValue(data.link);
        this.text.setValue(data.text);
        this.openInTab = data.openInTab;
    }

    getUrlErrorMessage() {
        if (this.link.hasError('required')) {
            return 'You must enter a value';
        }

        return this.link.hasError('pattern') ? 'Not a valid link' : '';
    }
    // Accept/cancel
    cancelChanges() {
        this.dialogRef.close({success: false});
    }

    acceptChanges() {
        this.dialogRef.close({success: true, operation: 'set-link', value: {
            link: this.link.value,
            text: this.text.value,
            openInTab: this.openInTab,
        }});
    }

    removeLink() {
        this.dialogRef.close({success: true, operation: 'remove-link'});
    }
}
