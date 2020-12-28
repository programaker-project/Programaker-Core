import { HttpClient } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { environment } from 'environments/environment';

@Injectable()
export class RenderedAboutResolver implements Resolve<string> {
    constructor(
        private httpClient: HttpClient,

    ) {}

    resolve(
        _route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<string | null> {
        if (environment.aboutPageRender) {
            return this.httpClient.get<string>(environment.aboutPageRender, { responseType: 'text' as 'json' }).toPromise();
        }
        else {
            return Promise.resolve(null);
        }
    }
}
