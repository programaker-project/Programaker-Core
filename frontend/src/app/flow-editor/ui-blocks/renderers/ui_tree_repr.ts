import { Area2D } from "../../flow_block";
import { BlockConfigurationOptions } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";

export type BackgroundPropertyConfiguration = { type: 'transparent' } | { type: 'color', value: string };

export type UiElementWidgetType
    = 'simple_button'
    | 'fixed_text'
    | 'dynamic_text'
    | 'fixed_image'
    | 'responsive_page_holder'
    | 'horizontal_ui_section'
    | 'horizontal_separator'
    | 'simple_card'
;

export type AtomicUiElementWidget
    = 'simple_button'
    | 'fixed_text'
    | 'dynamic_text'
    | 'fixed_image'
    | 'horizontal_separator'
;

export type UiElementWidgetContainer
    = 'simple_card'
;


export interface UiElementRepr {
    settings?: BlockConfigurationOptions;
    id: string,
    widget_type: AtomicUiElementWidget,
    text?: string,
    content?: any,
};

export interface ContainerElementRepr {
    id?: string,
    container_type: UiElementWidgetContainer,
    content: CutTree,
    background?: BackgroundPropertyConfiguration,
    // TODO: Should this background me moved into a settings property?
};

export interface CutElement {
    i: number,
    a: Area2D,
};
export type CutType = 'vbox' | 'hbox' | 'no-box';
export interface CutNode {
    background?: BackgroundPropertyConfiguration;
    cut_type: CutType,
    groups: CutTree[],
};
export type CutTree = UiElementRepr | ContainerElementRepr | CutNode;

export function isAtomicUiElementType(uiElement: UiElementWidgetType): uiElement is AtomicUiElementWidget {
    switch(uiElement) {
        case 'simple_button':
        case 'fixed_text':
        case 'dynamic_text':
        case 'fixed_image':
        case 'horizontal_separator':
            return true;

        default:
            return false;
    }
}

export function widgetAsAtomicUiElement(uiElement: UiElementWidgetType): AtomicUiElementWidget {
    if (isAtomicUiElementType(uiElement)) {
        return uiElement;
    }
    else {
        throw new Error(`Converted value is not an AtomicUiElement: ${uiElement}`);
    }
}
