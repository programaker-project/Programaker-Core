import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { HttpClientModule } from '@angular/common/http';
import { RouterModule } from '@angular/router';

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

import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { AlertModule } from 'ngx-bootstrap';

import { AppComponent } from './app.component';
import { DashboardComponent } from './dashboard.component';

import { ProgramsComponent } from './programs.component';
import { ProgramDetailComponent } from './program-detail.component';
import { AddServicesComponent } from './add-services.component';

import { ServicesComponent } from './services.component';

import { AppRoutingModule } from './app-routing.module';
import { LoginFormComponent } from './login-form.component';

import { SummarizeJSON } from './summarize_json.filter';
import { SelectFromJSON } from './select_from_json.filter';
import 'hammerjs';
import { HowToEnableServiceDialogComponent } from './HowToEnableServiceDialogComponent';
import { RenameProgramDialogComponent } from './RenameProgramDialogComponent';
import { DeleteProgramDialogComponent } from './DeleteProgramDialogComponent';

@NgModule({
    declarations: [
        AppComponent,
        DashboardComponent,
        ProgramsComponent,
        ProgramDetailComponent,
        AddServicesComponent,
        ServicesComponent,
        LoginFormComponent,

        // Dialogs
        HowToEnableServiceDialogComponent,
        RenameProgramDialogComponent,
        DeleteProgramDialogComponent,

        // Pipes
        SummarizeJSON,
        SelectFromJSON,
    ],
    imports: [
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

        BrowserAnimationsModule,

        BrowserModule,
        FormsModule,
        HttpModule,
        HttpClientModule,
        AlertModule.forRoot(),
        AppRoutingModule,
    ],
    exports: [
        AddServicesComponent,
    ],
    entryComponents: [
        HowToEnableServiceDialogComponent,
        RenameProgramDialogComponent,
        DeleteProgramDialogComponent,
    ],
    providers: [],
    bootstrap: [AppComponent],
})

export class AppModule {
    constructor() {
    }
}
