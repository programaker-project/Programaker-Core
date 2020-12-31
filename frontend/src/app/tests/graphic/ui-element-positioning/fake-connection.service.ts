import { Injectable } from "@angular/core";
import { BridgeIndexData } from "app/bridges/bridge";
import { BridgeConnection } from "app/connection";

@Injectable()
export class FakeConnectionService {
    constructor() {}

    public getAvailableBridgesForNewConnectionOnProgram(programId: string): Promise<BridgeIndexData[]> {
        return Promise.resolve([]);
    }

    public getConnectionsOnProgram(programId: string): Promise<BridgeConnection[]> {
        return Promise.resolve([]);
    }
}
