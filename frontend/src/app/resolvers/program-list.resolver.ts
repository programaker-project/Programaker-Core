import { ProgramMetadata } from "app/program";
import { Resolve, ActivatedRouteSnapshot, RouterStateSnapshot } from "@angular/router";
import { ProgramService } from "app/program.service";
import { Injectable } from "@angular/core";
import { GroupService } from "app/group.service";


@Injectable()
export class ProgramListResolver implements Resolve<ProgramMetadata[]> {
    constructor(
        private programService: ProgramService,
        private groupService: GroupService,
    ) {}

    async resolve(
        route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<ProgramMetadata[]> {
        const params = route.params;

        let asGroup: string | null = null;
        if (params.group_name) {
            const groupName = params.group_name;

            asGroup = (await this.groupService.getGroupWithName(groupName)).id;
        }

        let programListing: Promise<ProgramMetadata[]>;
        if (asGroup) {
            programListing = this.programService.getProgramsOnGroup(asGroup);
        }
        else {
            programListing = this.programService.getPrograms();
        }

        return programListing;
    }
}
