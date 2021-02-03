import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AdminService } from './admin.service';
import { DashboardComponent } from './dashboard/dashboard.component';
import { FlowEditorComponent } from './flow-editor/flow-editor.component';
import { GroupService } from './group.service';
import { AboutPageComponent } from './info-pages/about-page.component';
import { HomeRedirectComponent } from './info-pages/home-redirect.component';
import { LoginFormComponent } from './login-form/login-form.component';
import { RegisterFormComponent } from './login-form/register-form.component';
import { WaitForMailVerificationComponent } from './login-form/register-wait-for-mail-verification.component';
import { ResetPasswordStartComponent } from './login-form/reset-password-start.component';
import { ResetPasswordUpdatePasswordComponent } from './login-form/reset-password-update-password.component';
import { VerifyCodeComponent } from './login-form/verify-code.component';
import { NewGroupComponent } from './new/group/new-group.component';
import { ProgramDetailComponent } from './program-detail.component';
import { ProgramService } from './program.service';
import { AdminStatsResolver } from './resolvers/admin-stats.resolver';
import { AdminUserListResolver } from './resolvers/admin-user-list.resolver';
import { GroupInfoWithCollaboratorsResolver } from './resolvers/group-info-with-collaborators.resolver';
import { ProgramListResolver } from './resolvers/program-list.resolver';
import { RenderedAboutResolver } from './resolvers/rendered-about.resolver';
import { SessionResolver } from './resolvers/session.resolver';
import { SessionService } from './session.service';
import { UserProfileResolver } from './resolvers/user-profile.resolver';
import { AdminSettingsComponent } from './settings/admin-settings/admin-settings.component';
import { GroupSettingsComponent } from './settings/group-settings/group-settings.component';
import { SettingsComponent } from './settings/user-settings/settings.component';
import { UserProfileComponent } from './profiles/user-profile.component';
import { UserGroupsResolver } from './resolvers/user-groups.resolver';
import { UserBridgesResolver } from './resolvers/user-bridges.resolver';


const routes: Routes = [
    { path: '', component: HomeRedirectComponent, pathMatch: 'full' },
    { path: 'about', component: AboutPageComponent, resolve: { renderedAbout: RenderedAboutResolver } },

    { path: 'login', component: LoginFormComponent },
    { path: 'login/reset', component: ResetPasswordStartComponent },
    { path: 'login/reset/verify/:reset_verification_code', component: ResetPasswordUpdatePasswordComponent },
    { path: 'register', component: RegisterFormComponent },
    { path: 'register/wait_for_mail_verification', component: WaitForMailVerificationComponent },
    { path: 'register/verify/:verification_code', component: VerifyCodeComponent },

    // General
    { path: 'dashboard', component: DashboardComponent, resolve: { programs: ProgramListResolver } },
    { path: 'groups/:group_name', component: DashboardComponent, resolve: { programs: ProgramListResolver } },
    { path: 'groups/:group_name/settings', component: GroupSettingsComponent, resolve: { groupInfo: GroupInfoWithCollaboratorsResolver, session: SessionResolver } },

    // Profile pages
    { path: 'users/:user_name', component: UserProfileComponent, resolve: { user_profile: UserProfileResolver } },

    // Programs
    { path: 'users/:user_id/programs/:program_id', component: ProgramDetailComponent },
    { path: 'programs/:program_id/flow', component: FlowEditorComponent },
    { path: 'programs/:program_id/scratch', component: ProgramDetailComponent },

    // Bridges
    { path: 'bridges', redirectTo: '/dashboard#bridges' },

    // Settings
    { path: 'settings', component: SettingsComponent, resolve: { session: SessionResolver,
                                                                 groups: UserGroupsResolver,

                                                                 user_profile: UserProfileResolver,
                                                               } },
    { path: 'settings/admin', component: AdminSettingsComponent, resolve: { session: SessionResolver,
                                                                            adminStats: AdminStatsResolver, userList: AdminUserListResolver } },

    // Element creation
    { path: 'new/group', component: NewGroupComponent },

    // If no matching route found, go back to dashboard
    { path: '**', component: DashboardComponent, resolve: { programs: ProgramListResolver } },
];

@NgModule({
    imports: [ RouterModule.forRoot(routes, { initialNavigation: 'enabled' }) ],
    exports: [ RouterModule ],
    providers: [
        ProgramListResolver,
        SessionResolver,
        GroupInfoWithCollaboratorsResolver,
        AdminStatsResolver,
        AdminUserListResolver,
        RenderedAboutResolver,
        UserBridgesResolver,
        UserGroupsResolver,
        UserProfileResolver,

        AdminService,
        SessionService,
        ProgramService,
        GroupService,
    ]
})
export class AppRoutingModule {}
