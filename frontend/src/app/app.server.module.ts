import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { ANIMATION_MODULE_TYPE, NoopAnimationsModule } from '@angular/platform-browser/animations';
import { ServerModule } from '@angular/platform-server';
import { CookiesService } from '@ngx-utils/cookies';
import { ServerCookiesModule, ServerCookiesService } from '@ngx-utils/cookies/server';
import { AppComponent } from './app.component';
import { AppModule } from './app.module';

@NgModule({
    imports: [
        NoopAnimationsModule,
        BrowserModule.withServerTransition({appId: 'serverApp'}),
        ServerModule,
        ServerCookiesModule.forRoot(),
        AppModule,
    ],
    bootstrap: [AppComponent],
    providers: [
        {
            provide: CookiesService,
            useClass: ServerCookiesService,
        },
        {
            provide: ANIMATION_MODULE_TYPE,
            useValue: 'NoopAnimations',
        },
    ],
})
export class AppServerModule {}
