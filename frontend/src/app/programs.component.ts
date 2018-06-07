import { Component } from '@angular/core';
import { OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { ProgramMetadata } from './program';
// import { ProgramDetailComponent } from './program-detail.component';
import { ProgramService } from './program.service';

@Component({
    selector: 'app-my-programs',
    templateUrl: './programs.component.html',
    styleUrls: ['./app.component.css'],
    providers: [ProgramService]
})

export class ProgramsComponent implements OnInit {
    programs: ProgramMetadata[];
    selectedProgram: ProgramMetadata;

    constructor(
        private programService: ProgramService,
        private router: Router
    ) {
        this.programService = programService;
        this.router = router;
    }

    getPrograms() : Promise<ProgramMetadata[]> {
        return this.programService.getPrograms().then((programs) => this.programs = programs);
    }

    gotoDetail(): void {
        this.router.navigate(['/detail', this.selectedProgram.id]);
    }

    ngOnInit() : void {
        this.getPrograms();
    }

    gotoProgram(programId: number): void {
        this.router.navigate(['/programs/' + programId]);
    }

    onSelect(program: ProgramMetadata): void {
        this.selectedProgram = program;
    }

    addProgram() : void {
        this.router.navigate(['/programs/add']);
    }
}
