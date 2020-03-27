export type MessageType = 'integer' | 'boolean' | 'string' | 'any' | 'pulse';

export interface Position2D { x: number; y: number };

export interface OutputPortDefinition {
    type?: MessageType;
    name?: string;
}

export interface InputPortDefinition {
    type?: MessageType;
    name?: string;
}

export type OnIOSelected = ((block: FlowBlock,
                             type: 'in'|'out',
                             index: number,
                             definition: InputPortDefinition | OutputPortDefinition,
                             port_center: Position2D,
                            ) => void);

export interface FlowBlockOptions {
    message: string;
    title?: string;
    outputs?: OutputPortDefinition[];
    inputs?: InputPortDefinition[];
    on_io_selected?: OnIOSelected;
}

export interface FlowBlock {
    dispose(): void;
    render(canvas: SVGElement, position?: Position2D): SVGElement;

    getBodyElement(): SVGElement;

    getOffset(): Position2D;
    moveBy(distance: Position2D): void;

    getPositionOfInput(index: number, edge?: boolean): Position2D;
    getPositionOfOutput(index: number, edge?: boolean): Position2D;
    getOutputType(index: number): string;
}
