export interface SourceDefinition {
    block_id: string;
    output_index: number;
}

export interface SinkDefinition {
    block_id: string;
    input_index: number;
}

export type FlowConnectionData = {
    source: SourceDefinition,
    sink: SinkDefinition,
    id: string,
    type: string | null,
};


export function setConnectionType(connType: string, conn: FlowConnectionData, element: SVGElement) {
    conn.type = connType;

    let type_class = "unknown_wire";
    if (connType) {
        type_class = connType + '_wire';
    }

    element.setAttributeNS(null, 'class', 'established connection ' + type_class);
}
