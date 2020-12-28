import { HttpClient } from "@angular/common/http";
import { Injectable, PLATFORM_ID, Inject } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { environment } from 'environments/environment';
import { isPlatformServer } from "@angular/common";

@Injectable()
export class RenderedAboutResolver implements Resolve<string> {
    constructor(
        private httpClient: HttpClient,
        @Inject(PLATFORM_ID) private platformId: Object
    ) {}

    resolve(
        _route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<string | null> {
        let url = environment.aboutPageRender;

        if (environment.SSRAboutPageRender && isPlatformServer(this.platformId)) {
                // This cannot be rendered on server, so halt it's load
            url = environment.SSRAboutPageRender;
        }

        if (url) {
            return this.httpClient.get<string>(url, { responseType: 'text' as 'json' }).toPromise();
        }
        else {
            return Promise.resolve(null);
        }
    }
}
