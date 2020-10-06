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

export class AssetService {


    public static async GetTimezoneData() : Promise<TzItem[]> {
        // TODO: Replace with HttpClient
        const r = await fetch('/assets/timezones.json');
        return await r.json();
    }
}
