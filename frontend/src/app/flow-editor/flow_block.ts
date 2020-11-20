import { FlowWorkspace } from "./flow_workspace";

export type MessageType = 'integer' | 'float' | 'boolean' | 'string' | 'any' | 'pulse';

export interface Position2D { x: number; y: number };
export interface Area2D { x: number, y: number, width: number, height: number  };

export interface OutputPortDefinition {
    type?: MessageType;
    name?: string;
};

export interface PrimitiveTypeInputPortDefinition {
    type?: MessageType;
    name?: string;
    required?: boolean;
};

export interface BridgeEnumInputPortDefinition {
    type: 'enum';
    name?: string;
    enum_name: string;
    enum_namespace: string;
    required?: boolean;
};

export type InputPortDefinition = PrimitiveTypeInputPortDefinition | BridgeEnumInputPortDefinition;

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
    message?: string;
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
export type FlowBlockData = { type: string, value: any };
export interface FlowBlockInitOpts {
    position?: Position2D;
    block_id?: string;
    workspace?: FlowWorkspace;
};

export interface BlockContextAction {
    title: string,
    run: () => void,
};

export interface FlowBlock {
    dispose(): void;
    render(canvas: SVGElement, initOpts: FlowBlockInitOpts): SVGElement;
    serialize(): FlowBlockData;

    getBodyElement(): SVGElement;
    getBodyArea(): Area2D;

    getOffset(): Position2D;
    moveBy(distance: Position2D): void;

    addConnection(direction: 'in' | 'out', input: number, block: FlowBlock): void;
    removeConnection(direction: 'in' | 'out', index: number, block: FlowBlock): void;
    getBlockContextActions(): BlockContextAction[];

    getSlots(): {[key: string]: string};
    getInputs(): InputPortDefinition[];
    getPositionOfInput(index: number, edge?: boolean): Position2D;
    getPositionOfOutput(index: number, edge?: boolean): Position2D;
    getOutputType(index: number): string;
    getOutputRunwayDirection(): Direction2D;
}

export interface ContainerBlock {
    update(): void;
    removeContentBlock(block: FlowBlock): void;
    addContentBlock(block: FlowBlock): void;
}

export interface Resizeable {
    resize: (dimensions: { width: number, height: number }) => void;
    getBodyArea: () => Area2D;
}
