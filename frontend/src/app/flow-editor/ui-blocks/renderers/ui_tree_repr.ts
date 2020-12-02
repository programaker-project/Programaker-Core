import { Area2D } from "../../flow_block";

export type BackgroundPropertyConfiguration = { type: 'transparent' } | { type: 'color', value: string };

export type UiElementWidgetType
    = 'simple_button'
    | 'simple_debug_output'
    | 'responsive_page_holder'
    | 'horizontal_ui_section'
;

export interface UiElementRepr {
    id: string,
    widget_type: UiElementWidgetType,
    text?: string,
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
export type CutTree = UiElementRepr | CutNode;
