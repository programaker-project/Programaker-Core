export type MessageType = 'integer' | 'boolean' | 'string' | 'any' | 'pulse';

export interface Position2D { x: number; y: number };
export interface Area2D { x: number, y: number, width: number, height: number  };

export interface OutputPortDefinition {
    type?: MessageType;
    name?: string;
};

export interface InputPortDefinition {
    type?: MessageType;
    name?: string;
};

export interface ExtraInputDefinition {
    type: MessageType,
    quantity: 'any' | { max: number },
};

export type OnIOSelected = ((block: FlowBlock,
                             type: 'in'|'out',
                             index: number,
                             definition: InputPortDefinition | OutputPortDefinition,
                             port_center: Position2D,
                            ) => void);

export type OnInputsChanged = ((block: FlowBlock,
                                new_number: number,
                               ) => void);

export type OnDropdownExtended = ((block: FlowBlock,
                                   slot_id: string,
                                   previous_value: string,
                                   current_rect: Area2D,
                                   update: (new_value: string) => void,
                                  ) => void);

export interface FlowBlockOptions {
    message: string;
    title?: string;
    outputs?: OutputPortDefinition[];
    inputs?: InputPortDefinition[];
    extra_inputs?: ExtraInputDefinition;
    slots?: {[key: string]: string};

    on_io_selected?: OnIOSelected;
    on_inputs_changed?: OnInputsChanged;
    on_dropdown_extended?: OnDropdownExtended;
}

export type Direction2D = 'up' | 'down' | 'left' | 'right';

export interface FlowBlock {
    dispose(): void;
    render(canvas: SVGElement, position?: Position2D): SVGElement;

    getBodyElement(): SVGElement;
    getBodyArea(): Area2D;

    getOffset(): Position2D;
    moveBy(distance: Position2D): void;

    addConnection(direction: 'in' | 'out', input: number): void;
    removeConnection(direction: 'in' | 'out', index: number): void;

    getSlots(): {[key: string]: string};
    getInputs(): InputPortDefinition[];
    getPositionOfInput(index: number, edge?: boolean): Position2D;
    getPositionOfOutput(index: number, edge?: boolean): Position2D;
    getOutputType(index: number): string;
    getOutputRunwayDirection(): Direction2D;
}
