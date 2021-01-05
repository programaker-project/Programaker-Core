import { Area2D } from "../../flow_block";
import { BlockConfigurationOptions } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";
import { UiFlowBlock } from "../ui_flow_block";

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
    | 'link_area'
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
    | 'link_area'
;


export interface UiElementRepr {
    settings?: BlockConfigurationOptions;
    id: string,
    widget_type: AtomicUiElementWidget,
    text?: string,
    content?: any,
};

export interface ContainerElementRepr {
    id: string,
    container_type: UiElementWidgetContainer,
    content: CutTree,
    settings?: BlockConfigurationOptions,
};

export interface CutElement {
    i: number,
    a: Area2D,
    b: UiFlowBlock,
};
export type CutType = 'vbox' | 'hbox';

export const DEFAULT_CUT_TYPE = 'hbox';

export interface CutNode {
    settings?: BlockConfigurationOptions,
    block_id?: string,
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
