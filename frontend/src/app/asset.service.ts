import { HttpClient } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { EnvironmentService } from "./environment.service";
import { SessionService } from "./session.service";

export interface TzItem {
    country: string;
    dst_offset: string;
    latlong: string;
    notes: string;
    offset: string;
    region: string;
    status: "Alias" | "Canonical" | "Deprecated";
    tz: string;
}

@Injectable()
export class AssetService {
    constructor(
        private http: HttpClient,
        private environmentService: EnvironmentService,
        private sessionService: SessionService,
    ) { }

    public async getTimezoneData() : Promise<TzItem[]> {
        const r = await this.http.get('/assets/timezones.json').toPromise();
        return r as TzItem[];
    }

    public async copyAssetToProgram(sourceProgramId: string, assetId: string, destinationProgram: string): Promise<void> {
        const url = `${this.environmentService.getApiRoot()}/programs/by-id/${destinationProgram}/assets?copy_from=${sourceProgramId}/${assetId}`;
        const formData = new FormData();

        const result = await this.http.post(url, formData, {
            headers: this.sessionService.getAuthHeader()
        }).toPromise();
    }
}
