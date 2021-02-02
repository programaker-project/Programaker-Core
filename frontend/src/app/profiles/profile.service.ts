import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { BridgeIndexData } from 'app/bridges/bridge';
import { GroupInfo } from 'app/group';
import { EnvironmentService } from '../environment.service';
import { ProgramMetadata } from '../program';
import { SessionService } from '../session.service';
import { getUserPictureUrl, ground } from 'app/utils';


export type UserProfileInfo = {
    name: string,
    pictureUrl: string,
    programs: ProgramMetadata[],
    groups: GroupInfo[],
    bridges: BridgeIndexData[],
    id: string,
};

@Injectable()
export class ProfileService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
        private environmentService: EnvironmentService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    public async getProfileFromUsername(username: string): Promise<UserProfileInfo> {
        const url = `${this.environmentService.getApiRoot()}/users/by-name/${username}/profile`;

        const data : any = await this.http.get(url, { }).toPromise();

        data.pictureUrl = getUserPictureUrl(this.environmentService, data.id);
        data.groups = data.groups.map((group: GroupInfo) => ground(this.environmentService, group, 'picture'));

        return data as UserProfileInfo;
    }
}
