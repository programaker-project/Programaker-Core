import { AfterViewInit, Component, ElementRef, Inject, ViewChild } from '@angular/core';
import { MatButton } from '@angular/material/button';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatRadioChange, MatRadioGroup } from '@angular/material/radio';
import { SessionService } from '../../../session.service';
import { BackgroundPropertyConfiguration } from '../../ui-blocks/renderers/ui_tree_repr';

declare const Huebee: any;

export type ColorOption = { color: boolean };
export type ImageOption = { image?: boolean };
export type FontSizeOption = { fontSize: boolean };
export type ImageAssetConfiguration = {
    id: string,
};
export type WidthTakenOption = {widthTaken?: {name: string, style: string}[]};

export type SurfaceOption = ColorOption & ImageOption;
export type TextOptions = ColorOption & FontSizeOption;
export type BodyOptions = ImageOption & WidthTakenOption;

export type TextPropertyConfiguration = {
    color?: { value: string },
    fontSize?: { value: number },
};


export type BodyPropertyConfiguration = {
    image?: ImageAssetConfiguration,
    widthTaken?: { value: string },
}

export type BlockConfigurationOptions = {
    bg?: BackgroundPropertyConfiguration,
    text?: TextPropertyConfiguration,
    body?: BodyPropertyConfiguration,
};
export interface BlockAllowedConfigurations {
    background?: SurfaceOption,
    text?: TextOptions,
    body?: BodyOptions,
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
    providers: [ SessionService ],
    styleUrls: [
        './configure-block-dialog.component.scss',
        '../../../libs/css/material-icons.css',
    ],
})
export class ConfigureBlockDialogComponent implements AfterViewInit {
    loadedImage: File = null;
    selectedBackgroundType: 'color' | 'image' | 'transparent' = 'transparent';

    @ViewChild('acceptSaveConfigButton') acceptSaveConfigButton: MatButton;

    @ViewChild('bgColorPicker') bgColorPicker: ElementRef<HTMLInputElement>;
    @ViewChild('bgTypeSelector') bgTypeSelector: MatRadioGroup;
    private hbBgColorPicker: any;

    @ViewChild('textColorPicker') textColorPicker: ElementRef<HTMLInputElement>;
    @ViewChild('textSampleResult') textSampleResult: ElementRef<HTMLDivElement>;
    @ViewChild('fontSizeValueViewer') fontSizeValueViewer: ElementRef<HTMLElement>;
    @ViewChild('textFontSizePicker') textFontSizePicker: ElementRef<HTMLInputElement>;
    private hbTextColorPicker: any;

    @ViewChild('bodyImgPreview') bodyImgPreview: ElementRef<HTMLImageElement>;
    @ViewChild('bodyImgFileInput') bodyImgFileInput: ElementRef<HTMLInputElement>;
    bodyCurrentImage: string;
    @ViewChild('widthSampleResult') widthSampleResult: ElementRef<HTMLDivElement>;
    allowedWidthTypes: string[];
    widthTaken: string;

    currentConfig: BlockConfigurationOptions;
    allowedConfigurations: BlockAllowedConfigurations;

    // Initialization
    constructor(public dialogRef: MatDialogRef<ConfigureBlockDialogComponent>,
                private sessionService: SessionService,
                @Inject(MAT_DIALOG_DATA)
                public data: { programId: string, block: ConfigurableBlock }) {

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

        if (this.allowedConfigurations.body) {
            if (this.allowedConfigurations.body.widthTaken) {
                this.allowedWidthTypes = this.allowedConfigurations.body.widthTaken.map((x) => x.name );

                if (this.currentConfig.body && this.currentConfig.body.widthTaken) {
                    this.widthTaken = this.currentConfig.body.widthTaken.value;
                }
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

        if (this.currentConfig.body && this.currentConfig.body.widthTaken) {
            this.onNewWidthTaken({ value: this.currentConfig.body.widthTaken.value });
        }
    }

    onNewWidthTaken(change: { value: string }) {
        const val = this.allowedConfigurations.body.widthTaken.find(x => x.name == change.value);
        this.widthSampleResult.nativeElement.style.width = val.style;
    }

    onNewBgType(change: MatRadioChange) {
        if (change.value === 'color' && (!this.hbBgColorPicker)) {
            this._initBgColorPicker(DEFAULT_BACKGROUND_COLOR);
        }

        if (change.value === 'transparent' && (this.allowedConfigurations.text)) {
            this.textSampleResult.nativeElement.style.backgroundColor = 'transparent';
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
                    this.textSampleResult.nativeElement.style.backgroundColor = color;
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

    // Operation
    previewImage(event: KeyboardEvent) {
        const input: HTMLInputElement = event.target as HTMLInputElement;

        if (input.files && input.files[0]) {
            const reader = new FileReader();

            reader.onload = (e) => {
                this.loadedImage = input.files[0];
                this.bodyImgPreview.nativeElement.src = e.target.result as string;
            }

            reader.readAsDataURL(input.files[0]);
        }
    }


    // Accept/cancel
    cancelChanges() {
        this.dialogRef.close({success: false});
    }

    async acceptChanges() {
        const settings: BlockConfigurationOptions = {};

        const buttonClass = this.acceptSaveConfigButton._elementRef.nativeElement.classList;
        buttonClass.add('started');
        buttonClass.remove('completed');

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

        if (this.allowedConfigurations.body) {
            settings.body = {};
            if (this.loadedImage) {
                const imageId = (await this.sessionService.uploadAsset(this.loadedImage, this.data.programId)).value;
                settings.body.image = { id: imageId };
            }

            if (this.widthTaken) {
                settings.body.widthTaken = { value: this.widthTaken };
            }
        }

        buttonClass.remove('started');
        buttonClass.add('completed');

        this.dialogRef.close({success: true, settings: settings });
    }

}
