export type MessageType = 'integer' | 'string' | 'any';

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

    getPositionOfInput(index: number): Position2D;
    getPositionOfOutput(index: number): Position2D;
    getOutputType(index: number): string;
}
