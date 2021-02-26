import { FlowWorkspace } from "./flow_workspace";

export type MessageType = 'integer' | 'float' | 'boolean' | 'string' | 'any' | 'pulse' | 'user-pulse';

export interface Position2D { x: number; y: number };
export interface Area2D { x: number, y: number, width: number, height: number  };
export interface ManipulableArea2D { left: number, top: number, right: number, bottom: number  };

export interface Movement2D { x: number; y: number };

export interface OutputPortDefinition {
    type: MessageType;
    name?: string;
};

export interface PrimitiveTypeInputPortDefinition {
    type: MessageType;
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
export type FlowBlockData = { type: string, value: any, subtype?: string };
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
    updateOptions(blockData: FlowBlockData): void;
    moveTo(position: Position2D): void;
    onMove(callback: (pos: Position2D) => void): void;
    readonly id: string;
    dispose(): void;
    render(canvas: SVGElement, initOpts: FlowBlockInitOpts): SVGElement;
    serialize(): FlowBlockData;

    getBodyElement(): SVGElement;
    getBodyArea(): Area2D;

    getOffset(): Position2D;
    moveBy(distance: Position2D): FlowBlock[];
    endMove(): FlowBlock[];

    onGetFocus(): void;
    onLoseFocus(): void;

    addConnection(direction: 'in' | 'out', input: number, block: FlowBlock, sourceType: string): boolean;
    removeConnection(direction: 'in' | 'out', index: number, block: FlowBlock): boolean;
    getBlockContextActions(): BlockContextAction[];

    getSlots(): {[key: string]: string};
    getInputs(): InputPortDefinition[];
    getPositionOfInput(index: number, edge?: boolean): Position2D;
    getPositionOfOutput(index: number, edge?: boolean): Position2D;
    getOutputType(index: number): string;
    getInputType(index: number): string;
    getOutputRunwayDirection(): Direction2D;
}

export interface ContainerBlock {
    update(): void;
    removeContentBlock(block: FlowBlock): void;
    addContentBlock(block: FlowBlock): void;
    isPage: boolean;
    id: string;
}

export interface Resizeable {
    resize: (dimensions: { width: number, height: number }) => void;
    getBodyArea: () => Area2D;
}

export interface FlowActuator {
    onclick(): void;
    render(div: HTMLDivElement): HTMLElement;
}
