import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { ANIMATION_MODULE_TYPE, BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { AppComponent } from './app.component';
import { AppModule } from './app.module';

@NgModule({
    imports: [
        BrowserModule.withServerTransition({ appId: 'serverApp' }),
        BrowserCookiesModule.forRoot(),
        BrowserAnimationsModule,
        AppModule,
    ],
    bootstrap: [AppComponent],
    providers: [
        {
            provide: CookiesService,
            useClass: BrowserCookiesService,
        },
        {
            provide: ANIMATION_MODULE_TYPE,
            useValue: 'BrowserAnimations',
        },
    ],
})
export class AppBrowserModule {}
