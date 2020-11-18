import { Area2D } from "../../flow_block";


export type UiElementWidgetType
    = 'simple_button'
    | 'simple_debug_output'
    | 'responsive_page_holder'
;

export interface UiElementRepr {
    id: string,
    widget_type: UiElementWidgetType,
};

export interface CutElement {
    i: number,
    a: Area2D,
};
export type CutType = 'vertical' | 'horizontal' | 'no-cut';
export interface CutNode {
    cut_type: CutType,
    groups: CutTree[],
};
export type CutTree = UiElementRepr | CutNode;
