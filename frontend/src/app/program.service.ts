import { Injectable } from '@angular/core';
import { Program, ProgramExample } from './program';
import { Http } from '@angular/http';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';

@Injectable()
export class ProgramService {
    private getProgramsUrl = API.ApiRoot + '/programs/';
    private getExamplesForProgramRootUrl = '/api/programs/examples/';
    private addExampleToProgramRootUrl = '/api/programs/examples/';

    constructor(
        private http: Http,
    ) {
        this.http = http;
    }

    getPrograms(): Promise<Program[]> {
        return (this.http
                .get(this.getProgramsUrl)
                .map(response => response.json())
                .toPromise());
    }

    getProgram(id: number): Promise<Program> {
        return this.getPrograms()
            .then((programs) => programs.find((program) => program.id === id));
    }

    getExamplesForProgram(id: number): Promise<ProgramExample[]> {
        return (this.http
                .get(this.getExamplesForProgramRootUrl + id)
                .map(response => response.json())
                .toPromise());
    }

    persistExample(program: Program, example: ProgramExample): Promise<ProgramExample> {
        return (this.http.post(this.addExampleToProgramRootUrl + program.id, { text: example.text })
                .map(response => response.json())
                .toPromise());
    }
}
