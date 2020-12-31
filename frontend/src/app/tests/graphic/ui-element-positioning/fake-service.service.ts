import { Injectable } from "@angular/core";
import { AvailableService } from "app/service";

@Injectable()
export class FakeServiceService {
    constructor() {}

    getAvailableServicesOnProgram(programId: string): Promise<AvailableService[]> {
        return Promise.resolve([]);
    }
}
