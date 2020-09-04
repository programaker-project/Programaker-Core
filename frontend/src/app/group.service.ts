import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map, share } from 'rxjs/operators';
import { ApiRoot } from './api-config';
import { ContentType } from './content-type';
import { ProgramContent, ProgramEditorEvent, ProgramEditorEventValue, ProgramInfoUpdate, ProgramLogEntry, ProgramMetadata, ProgramType } from './program';
import { SessionService } from './session.service';
import { Synchronizer } from './syncronizer';


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

    async autocompleteUsers(query: string): Promise<UserAutocompleteInfo[]> {
        const url = this.getUserAutocompleteUrl();

        const result = await this.http.get(url, {
            headers: this.sessionService.getAuthHeader(),
            params: { q: query },
        }).toPromise();

        return result['users'];
    }

    async createGroup(name: string, options: { 'public': boolean, collaborators: string[] } ): Promise<string> {
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

        return result['id'];
    }
}
