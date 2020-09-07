import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map, share } from 'rxjs/operators';
import { ApiRoot } from './api-config';
import { ContentType } from './content-type';
import { ProgramContent, ProgramEditorEvent, ProgramEditorEventValue, ProgramInfoUpdate, ProgramLogEntry, ProgramMetadata, ProgramType } from './program';
import { SessionService } from './session.service';
import { Synchronizer } from './syncronizer';
import { GroupInfo } from './group';

export interface UserAutocompleteInfo {
    id: string,
    username: string,
};

@Injectable()
export class GroupService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    getUserAutocompleteUrl(): string {
        return `${ApiRoot}/utils/autocomplete/users`;
    }

    getCreateGroupUrl(): string {
        return `${ApiRoot}/groups`;
    }

    getGroupInfoUrl(groupName: string): string {
        return `${ApiRoot}/groups/by-name/${groupName}`;
    }

    async getUserGroupsUrl(): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId()
        return `${root}/groups`;
    }

    async autocompleteUsers(query: string): Promise<UserAutocompleteInfo[]> {
        const url = this.getUserAutocompleteUrl();

        const result = await this.http.get(url, {
            headers: this.sessionService.getAuthHeader(),
            params: { q: query },
        }).toPromise();

        return result['users'];
    }

    async createGroup(name: string, options: { 'public': boolean, collaborators: string[] } ): Promise<GroupInfo> {
        const url = this.getCreateGroupUrl();

        const result = await this.http.post(url,
                                            JSON.stringify({
                                                name: name,
                                                'public': options['public'],
                                                collaborators: options.collaborators,
                                            }),
                                            {
                                                headers: this.sessionService.addContentType(
                                                    this.sessionService.getAuthHeader(),
                                                    ContentType.Json)
                                            }).toPromise();

        return result['group'];
    }

    async getUserGroups(): Promise<GroupInfo[]> {
        const url = await this.getUserGroupsUrl();

        const result = await this.http.get(url, { headers: this.sessionService.getAuthHeader()})
            .toPromise();

        return result['groups'];
    }

    async getGroupWithName(groupName: any): Promise<GroupInfo> {
        const url = await this.getGroupInfoUrl(groupName);

        const result = await this.http.get(url, { headers: this.sessionService.getAuthHeader()})
            .toPromise();

        return result['group'];
    }

}
