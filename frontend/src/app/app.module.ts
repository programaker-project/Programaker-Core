import { HttpClientModule } from '@angular/common/http';
import { NgModule } from '@angular/core';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatBadgeModule } from '@angular/material/badge';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatChipsModule } from '@angular/material/chips';
import { MatRippleModule } from '@angular/material/core';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatMenuModule } from '@angular/material/menu';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatRadioModule } from '@angular/material/radio';
import { MatSelectModule } from '@angular/material/select';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { MatTabsModule } from '@angular/material/tabs';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatTooltipModule } from '@angular/material/tooltip'
import { MatStepperModule } from '@angular/material/stepper';

import { BrowserModule } from '@angular/platform-browser';
import { AlertModule } from 'ngx-bootstrap/alert';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { BridgeDeleteDialogComponent } from './bridges/delete-dialog.component';
import { GroupCollaboratorEditorComponent } from './components/group-collaborator-editor/group-collaborator-editor.component';
import { AddConnectionDialogComponent } from './connections/add-connection-dialog.component';
import { CustomSignalCreateDialogComponent } from './custom_signals/create-dialog.component';
import { DashboardComponent } from './dashboard/dashboard.component';
import { DeleteProgramDialogComponent } from './DeleteProgramDialogComponent';
import { AddBridgeDialogComponent } from './dialogs/add-bridge-dialog/add-bridge-dialog.component';
import { ConfirmDeleteDialogComponent } from './dialogs/confirm-delete-dialog/confirm-delete-dialog.component';
import { EditCollaboratorsDialogComponent } from './dialogs/editor-collaborators-dialog/edit-collaborators-dialog.component';
import { UpdateBridgeDialogComponent } from './dialogs/update-bridge-dialog/update-bridge-dialog.component';
import { ConfigureBlockDialogComponent } from './flow-editor/dialogs/configure-block-dialog/configure-block-dialog.component';
import { ConfigureFontColorDialogComponent } from './flow-editor/dialogs/configure-font-color-dialog/configure-font-color-dialog.component';
import { ConfigureLinkDialogComponent } from './flow-editor/dialogs/configure-link-dialog/configure-link-dialog.component';
import { FlowEditorComponent } from './flow-editor/flow-editor.component';
import { AboutPageComponent } from './info-pages/about-page.component';
// Info
import { HomeRedirectComponent } from './info-pages/home-redirect.component';
import { LoginFormComponent } from './login-form/login-form.component';
import { RegisterFormComponent } from './login-form/register-form.component';
import { WaitForMailVerificationComponent } from './login-form/register-wait-for-mail-verification.component';
import { ResetPasswordStartComponent } from './login-form/reset-password-start.component';
import { ResetPasswordUpdatePasswordComponent } from './login-form/reset-password-update-password.component';
import { VerifyCodeComponent } from './login-form/verify-code.component';
// Creation
import { NewGroupComponent } from './new/group/new-group.component';
// Programs
import { ProgramDetailComponent } from './program-detail.component';
import { SelectProgrammingModelDialogComponent } from './programs/select-programming-model-dialog/select-programming-model-dialog.component';
import { SetProgramTagsDialogComponent } from './program_tags/SetProgramTagsDialogComponent';
// Dialogs
import { RenameProgramDialogComponent } from './RenameProgramDialogComponent';
import { SelectFromJSON } from './select_from_json.filter';
// Services
import { ServicesComponent } from './services.component';
import { SessionService } from './session.service';
import { AdminSettingsComponent } from './settings/admin-settings/admin-settings.component';
import { GroupSettingsComponent } from './settings/group-settings/group-settings.component';
import { SettingsComponent } from './settings/user-settings/settings.component';
import { StopThreadProgramDialogComponent } from './StopThreadProgramDialogComponent';
import { SummarizeJSON } from './summarize_json.filter';
import { TemplateCreateDialogComponent } from './templates/create-dialog.component';
import { ProgramService } from './program.service';
import { UiSignalService } from './services/ui-signal.service';
import { ConnectionService } from './connection.service';
import { BridgeService } from './bridges/bridge.service';
import { CustomBlockService } from './custom_block.service';
import { CustomSignalService } from './custom_signals/custom_signal.service';
import { MonitorService } from './monitor.service';
import { ServiceService } from './service.service';
import { TemplateService } from './templates/template.service';
import { ChangeProgramVisilibityDialog } from './dialogs/change-program-visibility-dialog/change-program-visibility-dialog.component';
import { CloneProgramDialogComponent } from './dialogs/clone-program-dialog/clone-program-dialog.component';
import { AssetService } from './asset.service';
import { UserProfileComponent } from './profiles/user-profile.component';
import { ProfileService } from './profiles/profile.service';
import { GroupProfileComponent } from './profiles/group-profile.component';
import { ProgramEditorSidepanelComponent } from './components/program-editor-sidepanel/program-editor-sidepanel.component';
import { SpreadsheetEditorComponent } from './program-editors/spreadsheet-editor/spreadsheet-editor.component';
import { AuthorizeNewTokenComponent } from './components/authorize-new-token/authorize-new-token.component';



@NgModule({
    declarations: [
        AppComponent,
        DashboardComponent,
        SettingsComponent,
        AdminSettingsComponent,
        GroupSettingsComponent,
        ProgramDetailComponent,
        FlowEditorComponent,
        SpreadsheetEditorComponent,
        ServicesComponent,
        LoginFormComponent,
        ResetPasswordStartComponent,
        ResetPasswordUpdatePasswordComponent,
        RegisterFormComponent,
        WaitForMailVerificationComponent,
        VerifyCodeComponent,
        UserProfileComponent,
        GroupProfileComponent,

        // Info pages
        HomeRedirectComponent,
        AboutPageComponent,

        // Creation pages
        NewGroupComponent,
        AuthorizeNewTokenComponent,

        // Dialogs
        BridgeDeleteDialogComponent,
        RenameProgramDialogComponent,
        DeleteProgramDialogComponent,
        CustomSignalCreateDialogComponent,
        TemplateCreateDialogComponent,
        StopThreadProgramDialogComponent,
        SetProgramTagsDialogComponent,
        AddConnectionDialogComponent,
        EditCollaboratorsDialogComponent,
        AddBridgeDialogComponent,
        UpdateBridgeDialogComponent,
        ConfirmDeleteDialogComponent,
        ConfigureBlockDialogComponent,
        ConfigureLinkDialogComponent,
        ConfigureFontColorDialogComponent,
        ChangeProgramVisilibityDialog,
        CloneProgramDialogComponent,

        // Independent components
        GroupCollaboratorEditorComponent,
        ProgramEditorSidepanelComponent,

        // Pipes
        SummarizeJSON,
        SelectFromJSON,
        SelectProgrammingModelDialogComponent,
    ],
    imports: [
        MatAutocompleteModule,
        MatButtonModule,
        MatCheckboxModule,
        MatRadioModule,
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
        MatRippleModule,
        MatProgressSpinnerModule,
        MatBadgeModule,
        MatTooltipModule,
        MatSelectModule,
        MatStepperModule,

        FormsModule,
        ReactiveFormsModule,
        HttpClientModule,
        AlertModule.forRoot(),
        AppRoutingModule,

        BrowserModule.withServerTransition({ appId: 'serverApp' }),
    ],
    exports: [AppComponent],
    providers: [
        AssetService, BridgeService, ConnectionService, CustomBlockService, CustomSignalService,
        MonitorService, ProfileService, ProgramService, ServiceService, SessionService, TemplateService,
        UiSignalService,
    ],
    bootstrap: [],
})

export class AppModule {
    constructor() {
    }
}
