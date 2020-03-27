import { uuidv4 } from './utils';

interface SourceDefinition {
    block_id: string;
    output_index: number;
}

interface SinkDefinition {
    block_id: string;
    input_index: number;
}

export class FlowConnection {
    source: SourceDefinition;
    sink: SinkDefinition;
    id: string;

    constructor(from: SourceDefinition,
                to: SinkDefinition,
                id?: string) {

        this.source = { block_id: from.block_id, output_index: from.output_index };
        this.sink = { block_id: to.block_id, input_index: to.input_index };
        if (id) {
            this.id = id;
        }
        else {
            this.id = uuidv4();
        }
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
}
