import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { BridgeIndexComponent } from './bridges/index.component';
import { ServicesComponent } from './services.component';

import { BridgeAddComponent } from './bridges/add.component';

import { DashboardComponent } from './dashboard.component';

import { ProgramsComponent } from './programs.component';
import { ProgramDetailComponent } from './program-detail.component';

import { LoginFormComponent } from './login-form/login-form.component';
import { RegisterFormComponent } from './login-form/register-form.component';

import { HomeRedirectComponent } from './info-pages/home-redirect.component';
import { AboutPageComponent } from './info-pages/about-page.component';

const routes: Routes = [
    { path: '', component: HomeRedirectComponent, pathMatch: 'full' },
    { path: 'about', component: AboutPageComponent },

    { path: 'login', component: LoginFormComponent },
    { path: 'register', component: RegisterFormComponent },

    // General
    { path: 'dashboard', component: DashboardComponent },

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
