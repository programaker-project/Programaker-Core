import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { HttpClientModule } from '@angular/common/http';

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
import { MatSnackBarModule } from '@angular/material/snack-bar'
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';

import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { AlertModule } from 'ngx-bootstrap';

import { AppComponent } from './app.component';
import { DashboardComponent } from './dashboard.component';

// Programs
import { ProgramsComponent } from './programs.component';
import { ProgramDetailComponent } from './program-detail.component';
import { RenameProgramDialogComponent } from './RenameProgramDialogComponent';
import { DeleteProgramDialogComponent } from './DeleteProgramDialogComponent';
import { StopThreadProgramDialogComponent } from './StopThreadProgramDialogComponent';
import { SetProgramTagsDialogComponent } from './program_tags/SetProgramTagsDialogComponent';

// Services
import { ServicesComponent } from './services.component';
import { HowToEnableServiceDialogComponent } from './HowToEnableServiceDialogComponent';

// Bridges
import { BridgeIndexComponent } from './bridges/index.component';
import { BridgeAddComponent } from './bridges/add.component';
import { BridgeDeleteDialogComponent } from './bridges/delete-dialog.component';

// Custom signals
import { CustomSignalCreateDialogComponent } from './custom_signals/create-dialog.component';

// Templates
import { TemplateCreateDialogComponent } from './templates/create-dialog.component';

// Info
import { HomeRedirectComponent } from './info-pages/home-redirect.component';
import { AboutPageComponent } from './info-pages/about-page.component';


import { AppRoutingModule } from './app-routing.module';
import { LoginFormComponent } from './login-form/login-form.component';
import { RegisterFormComponent } from './login-form/register-form.component';
import { WaitForMailVerificationComponent } from './login-form/register-wait-for-mail-verification.component';
import { VerifyCodeComponent } from './login-form/verify-code.component';

import { SummarizeJSON } from './summarize_json.filter';
import { SelectFromJSON } from './select_from_json.filter';
import 'hammerjs';

@NgModule({
    declarations: [
        AppComponent,
        DashboardComponent,
        ProgramsComponent,
        ProgramDetailComponent,
        BridgeIndexComponent,
        BridgeAddComponent,
        ServicesComponent,
        LoginFormComponent,
        RegisterFormComponent,
        WaitForMailVerificationComponent,
        VerifyCodeComponent,

        // Info pages
        HomeRedirectComponent,
        AboutPageComponent,

        // Dialogs
        BridgeDeleteDialogComponent,
        HowToEnableServiceDialogComponent,
        RenameProgramDialogComponent,
        DeleteProgramDialogComponent,
        CustomSignalCreateDialogComponent,
        TemplateCreateDialogComponent,
        StopThreadProgramDialogComponent,
        SetProgramTagsDialogComponent,

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
        MatSnackBarModule,
        MatProgressSpinnerModule,

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
        BridgeAddComponent,
    ],
    entryComponents: [
        BridgeDeleteDialogComponent,
        HowToEnableServiceDialogComponent,
        RenameProgramDialogComponent,
        DeleteProgramDialogComponent,
        CustomSignalCreateDialogComponent,
        TemplateCreateDialogComponent,
        StopThreadProgramDialogComponent,
        SetProgramTagsDialogComponent,
    ],
    providers: [],
    bootstrap: [AppComponent],
})

export class AppModule {
    constructor() {
    }
}
