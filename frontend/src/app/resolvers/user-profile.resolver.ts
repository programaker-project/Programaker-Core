import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { ProfileService, UserProfileInfo } from "app/profiles/profile.service";

@Injectable()
export class UserProfileResolver implements Resolve<UserProfileInfo> {
    constructor(
        private profileService: ProfileService,
    ) {}

    async resolve(
        route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<UserProfileInfo> {
        const params = route.params;

        const userName = params.user_name;

        return this.profileService.getProfileFromUsername(userName)
    }
}
