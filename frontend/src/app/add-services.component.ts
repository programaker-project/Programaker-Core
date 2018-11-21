import { Component, OnInit } from '@angular/core';
import { Service, RequestInput, Request } from './service';
import { ServiceService } from './service.service';
import { Location } from '@angular/common';
import { Http } from '@angular/http';
import { GetTypeOfJson, JSONType } from './json';
import {MatSnackBar} from '@angular/material';
import { Router } from '@angular/router';

import { Session } from './session';
import { SessionService } from './session.service';


@Component({
    // moduleId: module.id,
    selector: 'app-my-add-services',
    templateUrl: './add-services.component.html',
    providers: [ServiceService],
})

export class AddServicesComponent {

    private testServiceUrl = 'https://plaza.spiral.systems/api/services/test';
    private createServiceUrl = 'https://plaza.spiral.systems/api/services/create';

    session: Session;
    service: Service;
    request: Request;
    currentFillingInput: string;
    currentFillingHeader: string;

    testExample: string;
    testFormat: string;
    resultPhrase: string;
    resolved: string;

    constructor(
        private location: Location,
        private http: Http,
        private snackbar: MatSnackBar,
        private router: Router,
        private sessionService: SessionService,
    ) {
        this.location = location;
        this.http = http;
        this.snackbar = snackbar;
        this.router = router;
        this.sessionService = sessionService;

        this.service = <Service>{ name: '' };
        this.request = <Request>{ url: '', method: 'GET',
                                  inputs: [], headers: []};
        this.currentFillingInput = '';
        this.currentFillingHeader = '';
        this.resultPhrase = '';
        this.resolved = '';
    }

    // tslint:disable-next-line:use-life-cycle-interface
    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (!session.active) {
                    this.router.navigate(['/login']);
                }
            });
    }

    goBack(): void {
        this.location.back();
    }

    selectMethodGet(): void {
        this.request.method = 'GET';
    }

    selectMethodPost(): void {
        this.request.method = 'POST';
    }

    addInput(): void {
        const input = this.currentFillingInput.trim();
        if (input.length > 0) {
            const newInput = <RequestInput>{
                name: input,
                defaultValue: '',
            };
            this.currentFillingInput = '';
            this.request.inputs.push(newInput);
        }
    }

    addHeader(): void {
        const input = this.currentFillingHeader.trim();
        if (input.length > 0) {
            const newInput = <RequestInput>{
                name: input,
                defaultValue: '',
            };
            this.currentFillingHeader = '';
            this.request.headers.push(newInput);
        }
    }


    toIndeterminateProgressbar(continueElement): void {
        continueElement.original = continueElement.innerHTML;
        continueElement.innerHTML = '...';
    }


    restoreFromIndeterminateProgressbar(continueElement): void {
        continueElement.innerHTML = continueElement.original;
    }


    getJSONSelector(element): string[] {
        const selector = [];
        while ((element.tagName !== 'BODY') && (element.getAttribute('key') !== '')) {
            if (element.getAttribute('key') !== null) {
                selector.push(element.getAttribute('key'));
            }

            element = element.parentNode;
        }

        selector.reverse();
        return selector;
    }


    selectorToString(selector: string[]): string {
        const fullSelector = ['element'].concat(selector);
        return (fullSelector
                .map(e => (e
                          .replace('>', '\\>')
                          .replace('}', '\\}')))
                .join('>'));
    }


    evalSelector(selector: string[], data: string): string {
        let step = data;
        for (let i = 1; i < selector.length; i++) {
            console.log(i, selector, step);
            step = step[selector[i]];
        }

        return step;
    }


    resolveSelectors(phrase: string, data: string): string {

        const result = [];
        for (let i = 0; i < phrase.length; i++) {
            if ((phrase[i] === '\\') && (phrase.length > i + 1)) {
                result.push(phrase[i + 1]);
                i++;
            } else if (phrase[i] !== '{') {
                result.push(phrase[i]);
            } else {
                const selectorParts = [];
                let selector = [];
                i++;

                for (; i < phrase.length; i++) {
                    if ((phrase[i] === '\\') && (phrase.length > i + 1)) {
                        selector.push(phrase[i + 1]);
                        i++;
                    } else if (phrase[i] === '>') {
                        selectorParts.push(selector.join(''));
                        selector = [];
                    } else if (phrase[i] !== '}') {
                        selector.push(phrase[i]);
                    } else {
                        break;
                    }
                }

                if (selector.length > 0) {
                    selectorParts.push(selector.join(''));
                    selector = [];
                }

                result.push(this.evalSelector(selectorParts, data));
            }
        }

        return result.join('');
    }


    addElement(element): void {
        const selector = this.getJSONSelector(element);
        console.log(selector);
        if (this.resultPhrase.length > 0) {
            this.resultPhrase += ' ';
        }

        this.resultPhrase += '{' + this.selectorToString(selector) + '}';
        console.log(this.selectorToString(selector));
        console.log(this.resultPhrase);
        const resolved = this.resolved = this.resolveSelectors(this.resultPhrase, this.testExample);
        this.snackbar.open(resolved, null, { duration: 2000, });
    }


    test(): void {
        const buttons = document.getElementsByClassName('continueButton');
        this.toIndeterminateProgressbar(buttons[0]);

        this.http.post(this.testServiceUrl, this.request)
            .subscribe(
                (response) => {
                    this.restoreFromIndeterminateProgressbar(buttons[0]);
                    this.testFormat = response.json().format;
                    this.testExample = response.json().example;
                },
                error => this.restoreFromIndeterminateProgressbar(buttons[0])
            );
    }


    create(): void {
        const buttons = document.getElementsByClassName('createButton');
        this.toIndeterminateProgressbar(buttons[0]);

        this.http.post(this.createServiceUrl, { request: this.request, service: this.service, result: this.resultPhrase })
            .subscribe(
                (response) => {
                    this.router.navigate(['/services/']);
                },
                error => this.restoreFromIndeterminateProgressbar(buttons[0])
            );
    }
}
