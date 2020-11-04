import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { SessionService } from 'app/session.service';
import { EnvironmentService } from 'app/environment.service';

@Injectable()
export class UiSignalService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
        private environmentService: EnvironmentService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    public async sendBlockSignal(blockType: string, blockId: string): Promise<void> {
        throw Error(`NotImplemented: Launch signal to services.ui.${blockType}.${blockId}`);
    }
}
