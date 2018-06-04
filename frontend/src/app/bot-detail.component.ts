import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { Bot } from './bot';
import { BotService } from './bot.service';
import 'rxjs/add/operator/switchMap';

@Component({
    selector: 'app-my-bot-detail',
    templateUrl: './bot-detail.component.html',
    providers: [BotService]
})

export class BotDetailComponent implements OnInit {
    @Input() bot: Bot;

    ngOnInit(): void {
        this.route.params
            .switchMap((params: Params) => this.botService.getBot(+params['id']))
            .subscribe(bot => this.bot = bot);
    }

    goBack(): void {
        this.location.back();
    }

    constructor (
        private botService: BotService,
        private route: ActivatedRoute,
        private location: Location
    ) {
        this.botService = botService;
        this.route = route;
        this.location = location;
    }
}
