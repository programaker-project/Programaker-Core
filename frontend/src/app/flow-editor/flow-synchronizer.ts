import { ProgramEditorEventValue } from "app/program";
import { Synchronizer } from "app/syncronizer";
import { Unsubscribable } from "rxjs";
import * as Y from 'yjs';

const TYPE_BUFFER_SIZE = 10;
type CircularBuffer<T> = { values: T[], idx: number };

export class FlowSynchronizer {
    private _typeBufferSize: number;
    private eventSubscription: Unsubscribable;
    hasChanges: boolean;

    constructor(private eventStream: Synchronizer<ProgramEditorEventValue>, 
                private doc: Y.Doc
                ) {

        this._typeBufferSize = TYPE_BUFFER_SIZE;

        this.eventSubscription = this.eventStream.subscribe({
            next: this.onNewEvent.bind(this),
            complete: () => {
                // clearTimeout(this._timeout)
            },
            error: () => {
                // clearTimeout(this._timeout)
            },
        });
        // this._timeout = setTimeout(this.onTimeoutCompleted.bind(this), FOLLOWER_WAIT_TIME);
    }

    private onNewEvent(ev: ProgramEditorEventValue) {
        // if (ev.type === CHECKPOINT_EVENT) {
        //     console.debug("Someone else is syncing...");
        //     clearTimeout(this._timeout);
        //     this._timeout = setTimeout(this.onTimeoutCompleted.bind(this), FOLLOWER_WAIT_TIME);
        // }
        // else {
        //    if (ev.save) {
                this.hasChanges = true;
        //    }
        // }
    }

    public close() {
        if (this.eventSubscription) {
            this.eventSubscription.unsubscribe();
            this.eventSubscription = null;
        }
        // if (this._timeout) {
        //     clearTimeout(this._timeout);
        //     this._timeout = null;
        // }
    }

}