import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { BridgeIndexData } from "app/bridges/bridge";
import { BridgeService } from "app/bridges/bridge.service";

@Injectable()
export class UserBridgesResolver implements Resolve<BridgeIndexData[]> {
    constructor(
        private bridgeService: BridgeService,
    ) {}

    async resolve(
        _route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<BridgeIndexData[]> {
        return (await this.bridgeService.listUserBridges()).bridges;
    }
}
