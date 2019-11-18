import { Component, ElementRef, ViewChild, Input, Inject } from '@angular/core';
import { COMMA, ENTER } from '@angular/cdk/keycodes';

import { FormControl } from '@angular/forms';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatAutocompleteSelectedEvent, MatAutocomplete } from '@angular/material/autocomplete';
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
    filteredTags: Observable<string[]>;
    allTags: string[] = [];
    tags: string[] = [];

    @ViewChild('tagInput') tagInput: ElementRef<HTMLInputElement>;
    @ViewChild('auto') matAutocomplete: MatAutocomplete;

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
        this.filteredTags = this.tagCtrl.valueChanges.pipe(
            startWith(null),
            map((tag: string | null) => tag ? this._filter(tag) : this.allTags.slice()));

        data.tags = this.tags;
        this.programService.getProgramTags(this.programUserId, this.program.id).then((tags) => {
            this.allTags = tags;

            // Bind data.tags again now that this.tags has changed
            data.tags = this.tags = tags;
        });
    }

    onNoClick(): void {
        this.dialogRef.close();
    }

    add(event: MatChipInputEvent): void {
        // Add tag only when MatAutocomplete is not open
        // To make sure this does not conflict with OptionSelected Event
        if (!this.matAutocomplete.isOpen) {
            const input = event.input;
            const value = event.value;

            // Add our tag
            if ((value || '').trim()) {
                this.tags.push(value.trim());
            }

            // Reset the input value
            if (input) {
                input.value = '';
            }

            this.tagCtrl.setValue(null);
        }
        console.log(this.data.tags, this.tags);
    }

    remove(tag: string): void {
        const index = this.tags.indexOf(tag);

        if (index >= 0) {
            this.tags.splice(index, 1);
        }
    }

    selected(event: MatAutocompleteSelectedEvent): void {
        this.tags.push(event.option.viewValue);
        this.tagInput.nativeElement.value = '';
        this.tagCtrl.setValue(null);
    }

    private _filter(value: string): string[] {
        const filterValue = value.toLowerCase();

        return this.allTags.filter(tag => tag.toLowerCase().indexOf(filterValue) === 0);
    }
}
