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
    private readonly _recentChangedBlocks: CircularBuffer<[string, string, string, any]>;
    private readonly _recentDeletedBlocks: CircularBuffer<string>;
    private readonly _recentMovedBlocks: CircularBuffer<[string, string, string, {x: number, y: number}]>;

    private readonly _recentCreatedVariables: CircularBuffer<string>;
    private readonly _recentDeletedVariables: CircularBuffer<string>;
    private readonly _recentRenamedVariables: CircularBuffer<[string, string]>;

    constructor() {
        this._typeBufferSize = TYPE_BUFFER_SIZE;
        this._recentCreatedBlocks = { values: [], idx: 0 };
        this._recentChangedBlocks = { values: [], idx: 0 };
        this._recentDeletedBlocks = { values: [], idx: 0 };
        this._recentMovedBlocks = { values: [], idx: 0 };

        this._recentCreatedVariables = { values: [], idx: 0 };
        this._recentDeletedVariables = { values: [], idx: 0 };
        this._recentRenamedVariables = { values: [], idx: 0 };
    }

    public receivedEvent(event: BlocklyEvent) {
        const classif = this.classifyEvent(event);

        if (classif === null) {
            return;
        }
        else if (!classif) {
            // This shouldn't happen unless this type is not controlled
            console.error(`Unexpected event type: ${event.type}`);
            return;
        }

        const [ bucket, value, _comp ] = classif;
        bucket.values[bucket.idx] = value;
        bucket.idx = (bucket.idx + 1) % this._typeBufferSize;
    }

    public isDuplicated(event: BlocklyEvent): boolean {
        const classif = this.classifyEvent(event);

        if (classif === null) {
            return false;
        }
        else if (!classif) {
            // This shouldn't happen unless this type is not controlled
            console.error(`Unexpected event type: ${event.type}`);
            return false;
        }

        const [ bucket, value, comp ] = classif;
        for (const existing of bucket.values) {
            if (comp(existing, value)) {
                return true;
            }
        }

        return false;
    }

    private classifyEvent(event: BlocklyEvent): [ CircularBuffer<any>, any, (x, y) => boolean ] | null {
        if (event instanceof Blockly.Events.BlockCreate) {
            return  [
                this._recentCreatedBlocks,
                event.blockId,
                (x, y) => x === y,
            ];
        }
        else if (event instanceof Blockly.Events.BlockChange) {
            // This MIGHT not be necessary and probably all events can be passed
            //  as changes to the already found value are to be ignored.
            return [
                this._recentChangedBlocks,
                [
                    event.blockId,
                    (event as any).element,
                    (event as any).name,
                    (event as any).newValue,
                ],
                ([a,b,c,d], [u,x,y,z]) => (
                    (a === u)
                        && (b === x)
                        && (c === y)
                        && (d === z)
                )
            ]
        }
        else if (event instanceof Blockly.Events.BlockDelete) {
            // This MIGHT not be necessary and probably all events can be passed
            //  as deletions over non-existing blocks cannot happen.
            return [
                this._recentDeletedBlocks,
                event.blockId,
                (x, y) => x === y,
            ]
        }
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
        else if (event instanceof Blockly.Events.VarCreate) {
            return  [
                this._recentCreatedVariables,
                event.varId,
                (x, y) => x === y,
            ];
        }
        else if (event instanceof Blockly.Events.VarDelete) {
            // This MIGHT not be necessary and probably all events can be passed
            //  as deletions over non-existing blocks cannot happen.
            return  [
                this._recentDeletedVariables,
                event.varId,
                (x, y) => x === y,
            ];
        }
        else if (event instanceof Blockly.Events.VarRename) {
            return  [
                this._recentRenamedVariables,
                [
                    event.varId,
                    (event as any).newName,
                ],
                ([a, b], [x, y]) => (
                    (a === x) && (b === y)
                ),
            ];
        }
        else if (event instanceof Blockly.Events.Ui) {
            // UI events are not to be reflected anyway
            return null;
        }
        else if (((event as any).type === 'endDrag')
            || ((event as any).type === 'dragOutside')) {

            // This might happen and is not bundled into Blockly.Events.Ui
            return null;
        }
    }
}
