import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { AddServicesComponent } from './add-services.component';
import { ServicesComponent } from './services.component';

import { DashboardComponent } from './dashboard.component';

import { ProgramsComponent } from './programs.component';
import { ProgramDetailComponent } from './program-detail.component';

import { LoginFormComponent } from './login-form.component';

const routes: Routes = [
    { path: '', redirectTo: '/dashboard', pathMatch: 'full' },

    { path: 'login', component: LoginFormComponent },

    // General
    { path: 'dashboard', component: DashboardComponent },

    // Programs
    { path: 'users/:user_id/programs/', component: ProgramsComponent },
    { path: 'users/:user_id/programs/:program_id', component: ProgramDetailComponent },

    // Services
    { path: 'services', component: ServicesComponent },
    { path: 'services/add', component: AddServicesComponent },
];

@NgModule({
    imports: [ RouterModule.forRoot(routes) ],
    exports: [ RouterModule ]
})
export class AppRoutingModule {}
