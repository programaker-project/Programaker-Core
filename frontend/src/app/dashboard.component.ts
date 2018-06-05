import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { Bot } from './bot';
import { BotService } from './bot.service';

import { Session } from './session';
import { SessionService } from './session.service';

@Component({
    // moduleId: module.id,
    selector: 'my-dashboard',
    templateUrl: './dashboard.component.html',
    providers: [BotService, SessionService]
})

export class DashboardComponent {
    bots: Bot[] = [];
    session: Session = null;

    constructor(
        private botService: BotService,
        private sessionService: SessionService,
        private router: Router,
    ) {
        this.botService = botService;
        this.sessionService = sessionService;
        this.router = router;
    }

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (!session.active) {
                    this.router.navigate(['/login']);
                } else {
                    this.botService.getBots()
                        .then(bots => this.bots = bots.slice(0, 4));
                }
            })
    }


    addBot(): void {
        this.router.navigate(['/bots/add']);
    }
}
