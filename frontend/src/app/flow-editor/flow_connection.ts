import { uuidv4 } from './utils';

export interface SourceDefinition {
    block_id: string;
    output_index: number;
}

export interface SinkDefinition {
    block_id: string;
    input_index: number;
}

export class FlowConnection {
    source: SourceDefinition;
    sink: SinkDefinition;
    id: string;
    connType: string | null;

    constructor(from: SourceDefinition,
                to: SinkDefinition,
                public readonly element: SVGElement,
                connType: string,
                id?: string) {

        this.source = { block_id: from.block_id, output_index: from.output_index };
        this.sink = { block_id: to.block_id, input_index: to.input_index };
        if (id) {
            this.id = id;
        }
        else {
            this.id = uuidv4();
        }

        this.setType(connType);
    }

    public getSource(): SourceDefinition {
        return { block_id: this.source.block_id, output_index: this.source.output_index };
    }

    public getSink(): SinkDefinition {
        return { block_id: this.sink.block_id, input_index: this.sink.input_index };
    }

    public getId(): string {
        return this.id;
    }

    public setType(connType: string) {
        this.connType = connType;

        let type_class = "unknown_wire";
        if (connType) {
            type_class = connType + '_wire';
        }

        this.element.setAttributeNS(null, 'class', 'established connection ' + type_class);
    }

    public getType(): string {
        return this.connType;
    }
}
