import { AfterViewInit, Component, ElementRef, Inject, ViewChild } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatRadioGroup, MatRadioChange } from '@angular/material/radio';
import { BackgroundPropertyConfiguration } from '../../ui-blocks/renderers/ui_tree_repr';

declare const Huebee: any;

export type ColorOption = { color: boolean };
export type ImageOption = { image: boolean };
export type FontSizeOption = { fontSize: boolean };

export type SurfaceOption = ColorOption & ImageOption;
export type TextOptions = ColorOption & FontSizeOption;

export type TextPropertyConfiguration = {
    color?: { value: string },
    fontSize?: { value: number },
};

export type BlockConfigurationOptions = {
    bg?: BackgroundPropertyConfiguration,
    text?: TextPropertyConfiguration,
};
export interface BlockAllowedConfigurations {
    background?: SurfaceOption,
    text?: TextOptions,
};

export interface ConfigurableBlock {
    applyConfiguration(settings: BlockConfigurationOptions): void;
    getCurrentConfiguration(): BlockConfigurationOptions;
    getAllowedConfigurations(): BlockAllowedConfigurations,
}

const DEFAULT_BACKGROUND_COLOR = '#FFFFFF';
const DEFAULT_TEXT_COLOR = '#000000';
const DEFAULT_TEXT_SIZE = 14;

@Component({
    selector: 'app-configure-block-dialog',
    templateUrl: './configure-block-dialog.component.html',
    styleUrls: [
        './configure-block-dialog.component.scss',
        '../../../libs/css/material-icons.css',
    ],
})
export class ConfigureBlockDialogComponent implements AfterViewInit {
    loadedImage: File = null;
    selectedBackgroundType: 'color' | 'image' | 'transparent' = 'transparent';

    @ViewChild('bgImgPreview') bgImgPreview: ElementRef<HTMLImageElement>;
    @ViewChild('bgImgFileInput') bgImgFileInput: ElementRef<HTMLInputElement>;
    @ViewChild('bgColorPicker') bgColorPicker: ElementRef<HTMLInputElement>;
    @ViewChild('bgTypeSelector') bgTypeSelector: MatRadioGroup;
    private hbBgColorPicker: any;

    @ViewChild('textColorPicker') textColorPicker: ElementRef<HTMLInputElement>;
    @ViewChild('textSampleResult') textSampleResult: ElementRef<HTMLDivElement>;
    @ViewChild('fontSizeValueViewer') fontSizeValueViewer: ElementRef<HTMLElement>;
    @ViewChild('textFontSizePicker') textFontSizePicker: ElementRef<HTMLInputElement>;
    private hbTextColorPicker: any;

    currentConfig: BlockConfigurationOptions;
    allowedConfigurations: BlockAllowedConfigurations;

    // Initialization
    constructor(public dialogRef: MatDialogRef<ConfigureBlockDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: { block: ConfigurableBlock }) {

        const config = this.allowedConfigurations = data.block.getAllowedConfigurations();

        this.currentConfig = data.block.getCurrentConfiguration();
        if (config.background) {
            if (this.currentConfig.bg) {
                this.selectedBackgroundType = this.currentConfig.bg.type;
            }
            else {
                this.currentConfig.bg = { type: 'transparent' };
            }
        }
    }

    ngAfterViewInit(): void {
        this.generateForm();
    }

    generateForm() {
        if (this.allowedConfigurations.background) {
            if (this.currentConfig.bg.type === 'color') {
                const color = this.currentConfig.bg.value || DEFAULT_BACKGROUND_COLOR;

                this._initBgColorPicker(color);
            }
        }

        if (this.allowedConfigurations.text) {
            const textConf = this.currentConfig.text;
            if (this.allowedConfigurations.text.color) {
                let color = DEFAULT_TEXT_COLOR;
                if (textConf && textConf.color) {
                    color = textConf.color.value;
                }

                this._initTextColorPicker(color);
            }

            if (this.allowedConfigurations.text.fontSize) {
                let size = DEFAULT_TEXT_SIZE;

                if (textConf && textConf.fontSize) {
                    size = textConf.fontSize.value;
                }

                this._initTextSizePicker(size);
            }
        }
    }

    onNewBgType(change: MatRadioChange) {
        if (change.value === 'color' && (!this.hbBgColorPicker)) {
            this._initBgColorPicker(DEFAULT_BACKGROUND_COLOR);
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

            if (this.allowedConfigurations.text) {
                this.textSampleResult.nativeElement.style.backgroundColor = startingColor;

                this.hbBgColorPicker.on('change', (color: string, _hue: any, _sat: any, _lum: any) => {
                    this.textSampleResult.nativeElement.style.color = color;
                });
            }
        }, 0); // Update this *after* the appropriate changes had happened
    }

    private _initTextColorPicker(startingColor: string) {
        setTimeout(() => {
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
        }, 0); // Update this *after* the appropriate changes had happened
    }

    private _initTextSizePicker(startingSize: number) {
        setTimeout(() => {
            // Font size selector
            this.textFontSizePicker.nativeElement.value = startingSize + '';
            this.fontSizeValueViewer.nativeElement.innerText = startingSize + '';

            this.textFontSizePicker.nativeElement.oninput = (ev) => {
                const value = (ev.srcElement as HTMLInputElement).value;

                this.fontSizeValueViewer.nativeElement.innerText = value;
                this.textSampleResult.nativeElement.style.fontSize = value + 'px';
            }
        }, 0); // Update this *after* the appropriate changes had happened

    }

    // Accept/cancel
    cancelChanges() {
        this.dialogRef.close({success: false});
    }

    acceptChanges() {
        const settings: BlockConfigurationOptions = {};

        if (this.allowedConfigurations.background) {
            if (this.selectedBackgroundType === 'color') {
                settings.bg = { type: 'color', value: this.hbBgColorPicker.color };
            }
            else if (this.selectedBackgroundType === 'transparent') {
                settings.bg = { type: 'transparent' };
            }
        }

        if (this.allowedConfigurations.text) {
            settings.text = {};
            if (this.allowedConfigurations.text.color) {
                settings.text.color = { value: this.hbTextColorPicker.color };
            }
            if (this.allowedConfigurations.text.fontSize) {
                settings.text.fontSize = { value: this.textFontSizePicker.nativeElement.valueAsNumber };
            }
        }

        this.dialogRef.close({success: true, settings: settings });
    }

}
