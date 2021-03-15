import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { Session } from "app/session";
import { SessionService } from "app/session.service";


@Injectable()
export class SessionResolver implements Resolve<Session> {
    constructor(
        private sessionService: SessionService,
    ) {}

    resolve(
        _route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<Session> {
        return this.sessionService.getSession();
    }
}
