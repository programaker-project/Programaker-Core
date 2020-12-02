import { AfterViewInit, Component, ElementRef, Inject, ViewChild } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatRadioGroup, MatRadioChange } from '@angular/material/radio';
import { BackgroundPropertyConfiguration } from 'app/flow-editor/ui-blocks/renderers/ui_tree_repr';

declare const Huebee: any;

export type SurfaceOptions = { color: boolean, image: boolean, current?: { image?: string, color?: string }};

export type BlockConfigurationOptions = { bg?: BackgroundPropertyConfiguration };

export interface ConfigurableBlock {
    applyConfiguration(settings: BlockConfigurationOptions): void;
    getCurrentConfiguration(): BlockConfigurationOptions;
    isBackgroundConfigurable?: false | SurfaceOptions,
}

const DEFAULT_COLOR = '#FFFFFF';

@Component({
    selector: 'app-configure-block-dialog',
    templateUrl: './configure-block-dialog.component.html',
    styleUrls: [
        './configure-block-dialog.component.scss',
        '../../../libs/css/material-icons.css',
    ],
})
export class ConfigureBlockDialogComponent implements AfterViewInit {
    bgConf: false | SurfaceOptions;

    loadedImage: File = null;
    selectedBackgroundType: 'color' | 'image' | 'transparent' = 'transparent';

    @ViewChild('bgImgPreview') bgImgPreview: ElementRef<HTMLImageElement>;
    @ViewChild('bgImgFileInput') bgImgFileInput: ElementRef<HTMLInputElement>;
    @ViewChild('bgColorPicker') bgColorPicker: ElementRef<HTMLInputElement>;
    @ViewChild('bgTypeSelector') bgTypeSelector: MatRadioGroup;
    private hbBgColorPicker: any;
    currentConfig: BlockConfigurationOptions;

    // Initialization
    constructor(public dialogRef: MatDialogRef<ConfigureBlockDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: { block: ConfigurableBlock }) {

        this.bgConf = data.block.isBackgroundConfigurable;
        if (this.bgConf && (!this.bgConf.image) && (!this.bgConf.color)) {
            this.bgConf = false;
        }


        this.currentConfig = data.block.getCurrentConfiguration();
        if (this.bgConf) {
            if (this.currentConfig.bg) {
                this.selectedBackgroundType = this.currentConfig.bg.type;
            }
        }
    }

    ngAfterViewInit(): void {
        this.generateForm();
    }

    generateForm() {
        if (this.bgConf) {
            if (this.currentConfig.bg.type === 'color') {
                const color = this.currentConfig.bg.value || DEFAULT_COLOR;

                this._initBgColorPicker(color);
            }
        }
    }

    onNewBgType(change: MatRadioChange) {
        if (change.value === 'color' && (!this.hbBgColorPicker)) {
            this._initBgColorPicker(DEFAULT_COLOR);
        }
    }

    private _initBgColorPicker(startingColor: string) {
        setTimeout(() => {
            this.hbBgColorPicker = new Huebee(this.bgColorPicker.nativeElement, {
                // options
                notation: 'hex',
                saturations: 2,
                setBGColor: true,
                staticOpen: true,
            });
            this.hbBgColorPicker.setColor(startingColor);
        }, 0); // Update this *after* the appropriate changes had happened
    }

    // Accept/cancel
    cancelChanges() {
        this.dialogRef.close({success: false});
    }

    acceptChanges() {
        const settings: BlockConfigurationOptions = {};

        if (this.bgConf) {
            if (this.selectedBackgroundType === 'color') {
                settings.bg = { type: 'color', value: this.hbBgColorPicker.color };
            }
            else if (this.selectedBackgroundType === 'transparent') {
                settings.bg = { type: 'transparent' };
            }
        }

        this.dialogRef.close({success: true, settings: settings });
    }

}
