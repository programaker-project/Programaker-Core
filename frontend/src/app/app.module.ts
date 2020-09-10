import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';

import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatIconModule } from '@angular/material/icon';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatCardModule } from '@angular/material/card';
import { MatInputModule } from '@angular/material/input';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatMenuModule } from '@angular/material/menu';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatDialogModule } from '@angular/material/dialog';
import { MatTabsModule } from '@angular/material/tabs';
import { MatChipsModule } from '@angular/material/chips';
import { MatSnackBarModule } from '@angular/material/snack-bar'
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatBadgeModule } from '@angular/material/badge';
import { MatTooltipModule } from '@angular/material/tooltip';

import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { AlertModule } from 'ngx-bootstrap/alert';

import { AppComponent } from './app.component';
import { DashboardComponent } from './old-dashboard/dashboard.component';
import { NewDashboardComponent } from './dashboard/dashboard.component';
import { GroupDashboardComponent } from './group-dashboard/dashboard.component';
import { SettingsComponent } from './settings/user-settings/settings.component';
import { AdminSettingsComponent } from './settings/admin-settings/admin-settings.component';
import { GroupSettingsComponent } from './settings/group-settings/group-settings.component';

// Programs
import { ProgramDetailComponent } from './program-detail.component';
import { FlowEditorComponent } from './flow-editor/flow-editor.component';

// Dialogs
import { RenameProgramDialogComponent } from './RenameProgramDialogComponent';
import { DeleteProgramDialogComponent } from './DeleteProgramDialogComponent';
import { StopThreadProgramDialogComponent } from './StopThreadProgramDialogComponent';
import { SetProgramTagsDialogComponent } from './program_tags/SetProgramTagsDialogComponent';
import { ConfirmDeleteDialogComponent } from './dialogs/confirm-delete-dialog/confirm-delete-dialog.component';
import { HowToEnableServiceDialogComponent } from './HowToEnableServiceDialogComponent';
import { BridgeDeleteDialogComponent } from './bridges/delete-dialog.component';
import { AddConnectionDialogComponent } from './connections/add-connection-dialog.component';
import { CustomSignalCreateDialogComponent } from './custom_signals/create-dialog.component';
import { TemplateCreateDialogComponent } from './templates/create-dialog.component';
import { SelectProgrammingModelDialogComponent } from './programs/select-programming-model-dialog/select-programming-model-dialog.component';
import { AddCollaboratorsDialogComponent } from './dialogs/add-collaborators-dialog/add-collaborators-dialog.component';

// Services
import { ServicesComponent } from './services.component';

// Bridges
import { BridgeIndexComponent } from './bridges/index.component';
import { BridgeAddComponent } from './bridges/add.component';

// Info
import { HomeRedirectComponent } from './info-pages/home-redirect.component';
import { AboutPageComponent } from './info-pages/about-page.component';

// Creation
import { NewGroupComponent } from './new/group/new-group.component';

import { AppRoutingModule } from './app-routing.module';
import { LoginFormComponent } from './login-form/login-form.component';
import { ResetPasswordStartComponent } from './login-form/reset-password-start.component';
import { ResetPasswordUpdatePasswordComponent } from './login-form/reset-password-update-password.component';
import { RegisterFormComponent } from './login-form/register-form.component';
import { WaitForMailVerificationComponent } from './login-form/register-wait-for-mail-verification.component';
import { VerifyCodeComponent } from './login-form/verify-code.component';

import { SummarizeJSON } from './summarize_json.filter';
import { SelectFromJSON } from './select_from_json.filter';

@NgModule({
    declarations: [
        AppComponent,
        DashboardComponent,
        NewDashboardComponent,
        GroupDashboardComponent,
        SettingsComponent,
        AdminSettingsComponent,
        GroupSettingsComponent,
        ProgramDetailComponent,
        FlowEditorComponent,
        BridgeIndexComponent,
        BridgeAddComponent,
        ServicesComponent,
        LoginFormComponent,
        ResetPasswordStartComponent,
        ResetPasswordUpdatePasswordComponent,
        RegisterFormComponent,
        WaitForMailVerificationComponent,
        VerifyCodeComponent,

        // Info pages
        HomeRedirectComponent,
        AboutPageComponent,

        // Creation pages
        NewGroupComponent,

        // Dialogs
        BridgeDeleteDialogComponent,
        HowToEnableServiceDialogComponent,
        RenameProgramDialogComponent,
        DeleteProgramDialogComponent,
        CustomSignalCreateDialogComponent,
        TemplateCreateDialogComponent,
        StopThreadProgramDialogComponent,
        SetProgramTagsDialogComponent,
        AddConnectionDialogComponent,
        AddCollaboratorsDialogComponent,
        ConfirmDeleteDialogComponent,

        // Pipes
        SummarizeJSON,
        SelectFromJSON,
        SelectProgrammingModelDialogComponent,
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
        MatBadgeModule,
        MatTooltipModule,
        MatSelectModule,

        BrowserAnimationsModule,

        BrowserModule,
        FormsModule,
        ReactiveFormsModule,
        HttpClientModule,
        AlertModule.forRoot(),
        AppRoutingModule,
    ],
    exports: [
        BridgeAddComponent,
    ],
    providers: [],
    bootstrap: [AppComponent],
})

export class AppModule {
    constructor() {
    }
}
