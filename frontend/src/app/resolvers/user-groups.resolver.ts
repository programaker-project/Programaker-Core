import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { GroupInfo } from "app/group";
import { GroupService } from "app/group.service";

@Injectable()
export class UserGroupsResolver implements Resolve<GroupInfo[]> {
    constructor(
        private groupService: GroupService,
    ) {}

    async resolve(
        _route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<GroupInfo[]> {
        return this.groupService.getUserGroups().catch(err => {
            console.error(err);
            return null;
        });
    }
}
