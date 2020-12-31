import { Injectable } from "@angular/core";
import { Observable, Observer } from "rxjs";

@Injectable()
export class FakeUiSignalService {
    programId: string;
    observers: Observer<any>[] = [];

    setProgramId(programId: string) {
        this.programId = programId;
    }

    public onElementUpdate(blockType: string, blockId: string): Observable<any> {
        return new Observable(observer => { this.observers.push(observer); });
    }
}
