import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { AddServicesComponent } from './add-services.component';
import { ServicesComponent } from './services.component';

import { DashboardComponent } from './dashboard.component';

import { AddProgramsComponent } from './add-programs.component';
import { ProgramsComponent } from './programs.component';
import { ProgramDetailComponent } from './program-detail.component';

import { AddBotComponent } from './add-bot.component';
import { BotDetailComponent } from './bot-detail.component';
import { LoginFormComponent } from './login-form.component';

const routes: Routes = [
    { path: '', redirectTo: '/dashboard', pathMatch: 'full' },

    { path: 'login', component: LoginFormComponent },

    // General
    { path: 'dashboard', component: DashboardComponent },

    // Bot detail
    { path: 'bots/add', component: AddBotComponent },
    { path: 'bots/:id', component: BotDetailComponent },

    // Programs
    { path: 'programs', component: ProgramsComponent },
    { path: 'programs/add', component: AddProgramsComponent },
    { path: 'programs/:id', component: ProgramDetailComponent },

    // Services
    { path: 'services', component: ServicesComponent },
    { path: 'services/add', component: AddServicesComponent },
];

@NgModule({
    imports: [ RouterModule.forRoot(routes) ],
    exports: [ RouterModule ]
})
export class AppRoutingModule {}
