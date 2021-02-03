import { Injectable } from "@angular/core";
import { ProgramContent, ProgramInfoUpdate } from "app/program";
import { Observable, Observer } from "rxjs";

const EMPTY_PROGRAM_GETTER: ((programId: string) => Promise<ProgramContent>)
    = (programId: string) => {
    return Promise.resolve({
        id: programId,
        name: 'empty program',
        enabled: true,
        bridges_in_use: [],
        is_public: false,

        type: 'flow_program',
        parsed: null,
        orig: { nodes: {}, edges: [] },
        owner: null,
        owner_full: { type: 'user', id: null },
    });
}

@Injectable()
export class FakeProgramService {
    private _programGetter: (programId: string) => Promise<ProgramContent>;
    private _observers: Observer<any>[] = [];

    constructor() {
        this._programGetter = EMPTY_PROGRAM_GETTER;
    }

    setProgramToBePulled(getter: ((programId: string) => Promise<ProgramContent>) ) {
        this._programGetter = getter;
    }

    getProgramById(programId: string): Promise<ProgramContent> {
         return this._programGetter(programId);
    }

    getAssetUrlOnProgram(assetId: string, programId: string): string {
        return `http://localhost:9999/programs/by-id/${programId}/assets/by-id/${assetId}`;
    }

    watchProgramLogs(programId: string, options: { request_previous_logs?: boolean }): Observable<ProgramInfoUpdate> {
        return new Observable<ProgramInfoUpdate>(observer => {
            this._observers.push(observer)
        });
    }
}
