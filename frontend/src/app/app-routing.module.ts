import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { BridgeIndexComponent } from './bridges/index.component';
import { ServicesComponent } from './services.component';

import { BridgeAddComponent } from './bridges/add.component';

import { DashboardComponent } from './dashboard.component';
import { ProfileComponent } from './profile/profile.component';

import { ProgramsComponent } from './programs.component';
import { ProgramDetailComponent } from './program-detail.component';

import { LoginFormComponent } from './login-form/login-form.component';
import { ResetPasswordStartComponent } from './login-form/reset-password-start.component';
import { ResetPasswordUpdatePasswordComponent } from './login-form/reset-password-update-password.component';
import { RegisterFormComponent } from './login-form/register-form.component';
import { WaitForMailVerificationComponent } from './login-form/register-wait-for-mail-verification.component';
import { VerifyCodeComponent } from './login-form/verify-code.component';

import { HomeRedirectComponent } from './info-pages/home-redirect.component';
import { AboutPageComponent } from './info-pages/about-page.component';

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
    { path: 'dashboard', component: DashboardComponent },
    { path: 'profile', component: ProfileComponent },

    // Programs
    { path: 'users/:user_id/programs/', component: ProgramsComponent },
    { path: 'users/:user_id/programs/:program_id', component: ProgramDetailComponent },

    // Services
    { path: 'services', component: ServicesComponent },

    // Bridges
    { path: 'bridges', component: BridgeIndexComponent },
    { path: 'bridges/add', component: BridgeAddComponent },
];

@NgModule({
    imports: [ RouterModule.forRoot(routes) ],
    exports: [ RouterModule ]
})
export class AppRoutingModule {}
