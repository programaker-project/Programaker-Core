import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { GroupInfo } from "app/group";
import { GroupService } from "app/group.service";
import { Collaborator } from "app/types/collaborator";

type Info = { info: GroupInfo, collaborators: Collaborator[]  };

@Injectable()
export class GroupInfoWithCollaboratorsResolver implements Resolve<Info> {
    constructor(
        private groupService: GroupService,
    ) {}

    async resolve(
        route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<Info> {
        const params = route.params;

        const groupName = params.group_name;

        try {
            const groupInfo = await this.groupService.getGroupWithName(groupName);
            const collaborators = await this.groupService.getCollaboratorsOnGroup(groupInfo.id);

            const result = {
                info: groupInfo,
                collaborators: collaborators,
            };

            return result;
        }
        catch(err) {
            console.error(err);
            return null;
        }
    }
}
