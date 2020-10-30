import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { AdminService, UserAdminData } from "app/admin.service";


@Injectable()
export class AdminUserListResolver implements Resolve<UserAdminData[]> {
    constructor(
        private adminService: AdminService,
    ) {}

    resolve(
        _route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<UserAdminData[]> {
        return this.adminService.listAllUsers();
    }
}
