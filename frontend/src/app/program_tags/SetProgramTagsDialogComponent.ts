import { Component, ElementRef, ViewChild, Input, Inject } from '@angular/core';
import { COMMA, ENTER } from '@angular/cdk/keycodes';

import { FormControl } from '@angular/forms';
import { MatChipInputEvent } from '@angular/material/chips';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

import { Observable } from 'rxjs';
import { map, startWith } from 'rxjs/operators';

import { ProgramMetadata } from '../program';
import { ProgramService } from '../program.service';
import { SessionService } from '../session.service';

@Component({
    selector: 'app-set-program-tags-dialog',
    templateUrl: 'set-program-tags-dialog.html',
    providers: [SessionService, ProgramService],
    styleUrls: [
        'set-program-tags-dialog.css',
        '../libs/css/material-icons.css',
    ]
})

export class SetProgramTagsDialogComponent {
    program: ProgramMetadata;
    program_name: string;
    programUserId: string;

    // Tag widget properties
    visible = true;
    selectable = true;
    removable = true;
    addOnBlur = true;
    separatorKeysCodes: number[] = [ENTER, COMMA];
    tagCtrl = new FormControl();
    tags: string[] = [];

    @ViewChild('tagInput', { static: true }) tagInput: ElementRef<HTMLInputElement>;

    constructor(public dialogRef: MatDialogRef<SetProgramTagsDialogComponent>,
                private programService: ProgramService,
                @Inject(MAT_DIALOG_DATA)
                public data: {
                    program: ProgramMetadata,
                    user_id: string,
                    tags: string[], // Output
                }) {

        this.programUserId = data.user_id;
        this.program = data.program;
        this.program_name = this.program.name;

        this.programService = programService;

        data.tags = this.tags;
        this.programService.getProgramTags(this.program.id).then((tags) => {
            // Bind data.tags again now that this.tags has changed
            data.tags = this.tags = tags;
        });
    }

    onNoClick(): void {
        this.dialogRef.close();
    }

    add(event: MatChipInputEvent): void {
        const input = event.input;
        const value = event.value;
        // Add our tag
        if ((value || '').trim()) {
            this.tags.push(value.trim());
            this.deduplicateTags();
        }

        // Reset the input value
        if (input) {
            input.value = '';
        }

        this.tagCtrl.setValue(null);
    }

    // Remove duplicated tags
    deduplicateTags(): void {
        const past = {};
        for (let index = 0; index < this.tags.length;) {
            const tag = this.tags[index];
            if (past[tag]) {
                // Duplicated, remove
                this.tags.splice(index, 1);
            }
            else {
                past[tag] = true;
                index++;
            }
        }
    }

    remove(tag: string): void {
        const index = this.tags.indexOf(tag);

        if (index >= 0) {
            this.tags.splice(index, 1);
        }
    }

}
