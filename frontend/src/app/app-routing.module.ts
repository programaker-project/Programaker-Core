import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { BridgeIndexComponent } from './bridges/index.component';
import { ServicesComponent } from './services.component';

import { BridgeAddComponent } from './bridges/add.component';

import { DashboardComponent } from './old-dashboard/dashboard.component';
import { NewDashboardComponent } from './dashboard/dashboard.component';
import { SettingsComponent } from './settings/user-settings/settings.component';
import { AdminSettingsComponent } from './settings/admin-settings/admin-settings.component';

import { ProgramDetailComponent } from './program-detail.component';
import { FlowEditorComponent } from './flow-editor/flow-editor.component';

import { LoginFormComponent } from './login-form/login-form.component';
import { ResetPasswordStartComponent } from './login-form/reset-password-start.component';
import { ResetPasswordUpdatePasswordComponent } from './login-form/reset-password-update-password.component';
import { RegisterFormComponent } from './login-form/register-form.component';
import { WaitForMailVerificationComponent } from './login-form/register-wait-for-mail-verification.component';
import { VerifyCodeComponent } from './login-form/verify-code.component';

import { HomeRedirectComponent } from './info-pages/home-redirect.component';
import { AboutPageComponent } from './info-pages/about-page.component';
import { NewGroupComponent } from './new/group/new-group.component';
import { GroupDashboardComponent } from './group-dashboard/dashboard.component';
import { GroupSettingsComponent } from './settings/group-settings/group-settings.component';

const routes: Routes = [
    { path: '', component: HomeRedirectComponent, pathMatch: 'full' },
    { path: 'about', component: AboutPageComponent },

    { path: 'login', component: LoginFormComponent },
    { path: 'login/reset', component: ResetPasswordStartComponent },
    { path: 'login/reset/verify/:reset_verification_code', component: ResetPasswordUpdatePasswordComponent },
    { path: 'register', component: RegisterFormComponent },
    { path: 'register/wait_for_mail_verification', component: WaitForMailVerificationComponent },
    { path: 'register/verify/:verification_code', component: VerifyCodeComponent },

    // General
    { path: 'full-dashboard', component: DashboardComponent },
    { path: 'dashboard', component: NewDashboardComponent },
    { path: 'groups/:group_name', component: GroupDashboardComponent },
    { path: 'groups/:group_name/settings', component: GroupSettingsComponent },

    // Programs
    { path: 'users/:user_id/programs/:program_id', component: ProgramDetailComponent },
    { path: 'programs/:program_id/flow', component: FlowEditorComponent },
    { path: 'programs/:program_id/scratch', component: ProgramDetailComponent },

    // Services
    { path: 'services', component: ServicesComponent },

    // Bridges
    { path: 'bridges', component: BridgeIndexComponent },
    { path: 'bridges/add', component: BridgeAddComponent },

    // Settings
    { path: 'settings', component: SettingsComponent },
    { path: 'settings/admin', component: AdminSettingsComponent },

    // Element creation
    { path: 'new/group', component: NewGroupComponent },
];

@NgModule({
    imports: [ RouterModule.forRoot(routes) ],
    exports: [ RouterModule ]
})
export class AppRoutingModule {}
