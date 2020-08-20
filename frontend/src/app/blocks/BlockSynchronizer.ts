/// <reference path="./blockly-core.d.ts" />

export type BlocklyEvent = (
    Blockly.Events.BlockCreate
        | Blockly.Events.BlockChange
        | Blockly.Events.BlockDelete
        | Blockly.Events.BlockMove
        | Blockly.Events.VarCreate
        | Blockly.Events.VarDelete
        | Blockly.Events.VarRename
        | Blockly.Events.Ui
) & { type: string }
;

const TYPE_BUFFER_SIZE = 10;
type CircularBuffer<T> = { values: T[], idx: number };

export class BlockSynchronizer {
    private readonly _typeBufferSize: number;
    private readonly _recentCreatedBlocks: CircularBuffer<string>;
    private readonly _recentMovedBlocks: CircularBuffer<[string, string, string, {x: number, y: number}]>;

    constructor() {
        this._typeBufferSize = TYPE_BUFFER_SIZE;
        this._recentCreatedBlocks = { values: [], idx: 0 };
        this._recentMovedBlocks = { values: [], idx: 0 };
    }

    public receivedEvent(event: BlocklyEvent) {
        const [ bucket, value, _comp ] = this.classifyEvent(event);

        bucket.values[bucket.idx] = value;
        bucket.idx = (bucket.idx + 1) % this._typeBufferSize;
    }

    public isDuplicated(event: BlocklyEvent): boolean {
        const [ bucket, value, comp ] = this.classifyEvent(event);

        for (const existing of bucket.values) {
            if (comp(existing, value)) {
                return true;
            }
        }

        return false;
    }

    private classifyEvent(event: BlocklyEvent): [ CircularBuffer<any>, any, (x, y) => boolean ] {
        if (event instanceof Blockly.Events.BlockCreate) {
            return  [
                this._recentCreatedBlocks,
                event.blockId,
                (x, y) => x === y,
            ];
        }
        // else if (event instanceof Blockly.Events.BlockChange) {
        // }
        // else if (event instanceof Blockly.Events.BlockDelete) {
        // }
        else if (event instanceof Blockly.Events.BlockMove) {
            return [
                this._recentMovedBlocks,
                [
                    event.blockId,
                    (event as any).newParentId,
                    (event as any).newInputName,
                    (event as any).newCoordinate,
                ],
                ([a,b,c,d], [u,x,y,z]) => (
                    (a === u)
                        && (b === x)
                        && (c === y)
                        && (d.x === z.x)
                        && (d.y === z.y)
                )
            ]
        }
        // else if (event instanceof Blockly.Events.VarCreate) {
        // }
        // else if (event instanceof Blockly.Events.VarDelete) {
        // }
        // else if (event instanceof Blockly.Events.VarRename) {
        // }
        else {
            // Not match, never
            return [
                { values: [], idx: 0 },
                null,
                (x, y) => false,
            ]
        }
    }

}
