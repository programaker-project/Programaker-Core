import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { AdminService, PlatformStatsInfo } from "app/admin.service";


@Injectable()
export class AdminStatsResolver implements Resolve<PlatformStatsInfo> {
    constructor(
        private adminService: AdminService,
    ) {}

    resolve(
        _route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<PlatformStatsInfo> {
        return this.adminService.getStats().catch(err => {
            console.error(err);
            return null;
        });
    }
}
