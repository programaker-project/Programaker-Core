/// <reference path="./blockly-core.d.ts" />

import { Synchronizer } from "app/syncronizer";
import { ProgramEditorEventValue } from "app/program";
import { Unsubscribable } from "rxjs";

const CHECKPOINT_EVENT = 'save_checkpoint';
const LEADER_WAIT_TIME = 60 * 1000;
const FOLLOWER_WAIT_TIME = LEADER_WAIT_TIME * 2;

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

    private readonly _eventStream: Synchronizer<ProgramEditorEventValue>;
    private readonly _syncStatus: () => Promise<void>;
    private hasChanges: boolean = false;
    private _timeout: NodeJS.Timeout;
    eventSubscription: Unsubscribable;

    constructor(eventStream: Synchronizer<ProgramEditorEventValue>, syncStatus: () => Promise<void>) {
        this._typeBufferSize = TYPE_BUFFER_SIZE;
        this._recentCreatedBlocks = { values: [], idx: 0 };
        this._recentChangedBlocks = { values: [], idx: 0 };
        this._recentDeletedBlocks = { values: [], idx: 0 };
        this._recentMovedBlocks = { values: [], idx: 0 };

        this._recentCreatedVariables = { values: [], idx: 0 };
        this._recentDeletedVariables = { values: [], idx: 0 };
        this._recentRenamedVariables = { values: [], idx: 0 };

        this._eventStream = eventStream;
        this._syncStatus = syncStatus;

        this.eventSubscription = this._eventStream.subscribe({
            next: this.onNewEvent.bind(this),
            complete: () => {
                clearTimeout(this._timeout)
            },
            error: () => {
                clearTimeout(this._timeout)
            },
        });
        this._timeout = setTimeout(this.onTimeoutCompleted.bind(this), FOLLOWER_WAIT_TIME);
    }

    private onNewEvent(ev: ProgramEditorEventValue) {
        if (ev.type === CHECKPOINT_EVENT) {
            console.debug("Someone else is syncing...");
            clearTimeout(this._timeout);
            this._timeout = setTimeout(this.onTimeoutCompleted.bind(this), FOLLOWER_WAIT_TIME);
        }
        else {
            if (ev.save) {
                this.hasChanges = true;
            }
        }
    }

    private async onTimeoutCompleted() {
        console.debug("Syncing...");
        this._eventStream.push({ type: CHECKPOINT_EVENT, save: false, value: {} });

        if (!this.hasChanges)  {
            console.debug("Skipping due to no changes");

        }
        else {
            try {
                await this._syncStatus();
                this.hasChanges = false;
            }
            catch(error) {
                console.error("Sync error:", error);
            }
        }

        this._timeout = setTimeout(this.onTimeoutCompleted.bind(this), LEADER_WAIT_TIME);
    }

    public receivedEvent(event: BlocklyEvent) {
        const classif = this.classifyEvent(event);
        this.hasChanges = true;

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
            this.hasChanges = true;
            return false;
        }

        const [ bucket, value, comp ] = classif;
        for (const existing of bucket.values) {
            if (comp(existing, value)) {
                return true;
            }
        }

        this.hasChanges = true;
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
                        && ((!d && !z)
                            || ((d.x === z.x)
                                && (d.y === z.y)))
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

    public close() {
        if (this.eventSubscription) {
            this.eventSubscription.unsubscribe();
            this.eventSubscription = null;
        }
        if (this._timeout) {
            clearTimeout(this._timeout);
            this._timeout = null;
        }
    }
}
