import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { HttpClientModule } from '@angular/common/http';
import { RouterModule } from '@angular/router';

import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatButtonModule, MatCheckboxModule } from '@angular/material';
import { MatIconModule } from '@angular/material/icon';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatCardModule } from '@angular/material/card';
import { MatInputModule } from '@angular/material/input';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatMenuModule } from '@angular/material/menu';
import { MatFormFieldModule } from '@angular/material';
import { MatDialogModule } from '@angular/material/dialog';
import { MatTabsModule } from '@angular/material/tabs';
import { MatChipsModule } from '@angular/material/chips';

import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { AlertModule } from 'ngx-bootstrap';

import { AppComponent } from './app.component';
import { DashboardComponent } from './dashboard.component';

// Programs
import { ProgramsComponent } from './programs.component';
import { ProgramDetailComponent } from './program-detail.component';
import { RenameProgramDialogComponent } from './RenameProgramDialogComponent';
import { DeleteProgramDialogComponent } from './DeleteProgramDialogComponent';
import { ProgramMetadataComponent } from './program_metadata/program-metadata.component';

// Services
import { AddServicesComponent } from './add-services.component';
import { ServicesComponent } from './services.component';
import { HowToEnableServiceDialogComponent } from './HowToEnableServiceDialogComponent';

// Bridges
import { BridgeIndexComponent } from './bridges/index.component';
import { BridgeAddComponent } from './bridges/add.component';
import { BridgeDeleteDialogComponent } from './bridges/delete-dialog.component';

// Templates
import { TemplateCreateDialogComponent } from './templates/create-dialog.component';

import { AppRoutingModule } from './app-routing.module';
import { LoginFormComponent } from './login-form.component';

import { SummarizeJSON } from './summarize_json.filter';
import { SelectFromJSON } from './select_from_json.filter';
import 'hammerjs';

@NgModule({
    declarations: [
        AppComponent,
        DashboardComponent,
        ProgramsComponent,
        ProgramDetailComponent,
        ProgramMetadataComponent,
        AddServicesComponent,
        BridgeIndexComponent,
        BridgeAddComponent,
        ServicesComponent,
        LoginFormComponent,

        // Dialogs
        BridgeDeleteDialogComponent,
        HowToEnableServiceDialogComponent,
        RenameProgramDialogComponent,
        DeleteProgramDialogComponent,
        TemplateCreateDialogComponent,

        // Pipes
        SummarizeJSON,
        SelectFromJSON,
    ],
    imports: [
        MatAutocompleteModule,
        MatButtonModule,
        MatCheckboxModule,
        MatIconModule,
        MatToolbarModule,
        MatSidenavModule,
        MatCardModule,
        MatInputModule,
        MatSlideToggleModule,
        MatMenuModule,
        MatFormFieldModule,
        MatDialogModule,
        MatTabsModule,
        MatChipsModule,

        BrowserAnimationsModule,

        BrowserModule,
        FormsModule,
        ReactiveFormsModule,
        HttpModule,
        HttpClientModule,
        AlertModule.forRoot(),
        AppRoutingModule,
    ],
    exports: [
        AddServicesComponent,
        BridgeAddComponent,
    ],
    entryComponents: [
        BridgeDeleteDialogComponent,
        HowToEnableServiceDialogComponent,
        RenameProgramDialogComponent,
        DeleteProgramDialogComponent,
        TemplateCreateDialogComponent,
    ],
    providers: [],
    bootstrap: [AppComponent],
})

export class AppModule {
    constructor() {
    }
}
