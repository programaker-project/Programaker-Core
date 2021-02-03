import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { BridgeIndexData } from 'app/bridges/bridge';
import { GroupInfo } from 'app/group';
import { EnvironmentService } from '../environment.service';
import { ProgramMetadata } from '../program';
import { SessionService } from '../session.service';
import { getUserPictureUrl, ground, getGroupPictureUrl } from 'app/utils';


export type UserProfileInfo = {
    name: string,
    pictureUrl: string,
    programs: ProgramMetadata[],
    groups: GroupInfo[],
    bridges: BridgeIndexData[],
    id: string,
};

export type GroupProfileInfo = {
    name: string,
    pictureUrl: string,
    programs: ProgramMetadata[],
    collaborators: { name: string, id: string, picture?: string }[],
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

    public async getProfileFromGroupname(groupname: string): Promise<GroupProfileInfo> {
        const url = `${this.environmentService.getApiRoot()}/groups/by-name/${groupname}/profile`;

        const data : any = await this.http.get(url, {headers: this.sessionService.getAuthHeader()}).toPromise();

        data.pictureUrl = getGroupPictureUrl(this.environmentService, data.id);
        data.collaborators.forEach((collaborator: { name: string, picture: string; id: string; }) => {
            collaborator.picture = getUserPictureUrl(this.environmentService, collaborator.id)
        });

        return data as GroupProfileInfo;
    }
}
