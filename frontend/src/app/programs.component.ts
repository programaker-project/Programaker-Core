import { Component } from '@angular/core';
import { OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Program } from './program';
// import { ProgramDetailComponent } from './program-detail.component';
import { ProgramService } from './program.service';

@Component({
    selector: 'my-programs',
    templateUrl: './programs.component.html',
    styleUrls: ['./app.component.css'],
    providers: [ProgramService]
})

export class ProgramsComponent implements OnInit {
    programs: Program[];
    selectedProgram: Program;

    constructor(
        private programService: ProgramService,
        private router: Router
    ) {
        this.programService = programService;
        this.router = router;
    }

    getPrograms() : Promise<Program[]> {
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

    onSelect(program: Program): void {
        this.selectedProgram = program;
    }

    addProgram() : void {
        this.router.navigate(['/programs/add']);
    }
}
