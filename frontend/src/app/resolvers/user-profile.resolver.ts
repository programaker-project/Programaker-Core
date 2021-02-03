import { Injectable } from "@angular/core";
import { ActivatedRouteSnapshot, Resolve, RouterStateSnapshot } from "@angular/router";
import { ProfileService, UserProfileInfo } from "app/profiles/profile.service";
import { SessionService } from "app/session.service";

@Injectable()
export class UserProfileResolver implements Resolve<UserProfileInfo> {
    constructor(
        private profileService: ProfileService,
        private sessionService: SessionService,
    ) {}

    async resolve(
        route: ActivatedRouteSnapshot,
        _state: RouterStateSnapshot
    ): Promise<UserProfileInfo> {
        const params = route.params;

        const userName = params.user_name ? params.user_name : (await this.sessionService.getSession()).username;

        if (!userName) {
            return Promise.resolve(null);
        }

        return this.profileService.getProfileFromUsername(userName).catch(err => {
            console.error(err);
            return null;
        });
    }
}
