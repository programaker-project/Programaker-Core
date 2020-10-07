import { HttpClient } from "@angular/common/http";
import { Injectable } from "@angular/core";

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
    ) { }

    public async getTimezoneData() : Promise<TzItem[]> {
        const r = await this.http.get('/assets/timezones.json').toPromise();
        return r as TzItem[];
    }
}
