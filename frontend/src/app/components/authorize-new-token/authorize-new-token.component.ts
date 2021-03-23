import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { environment } from 'environments/environment';
import { EnvironmentDefinition } from 'environments/environment-definition';
import { ToastrService } from 'ngx-toastr';


const SCOPE_DESCRIPTIONS: {[key: string]: string} = {
    'list_bridges': "List user's bridges.",
    'call_any_bridge': "Invoke any bridge with the user's credentials.",
};

@Component({
    selector: 'app-authorize-new-token',
    templateUrl: './authorize-new-token.component.html',
    styleUrls: [
        './authorize-new-token.component.scss',
        '../../libs/css/material-icons.css',
    ],
    providers: [SessionService, ToastrService],
})
export class AuthorizeNewTokenComponent implements OnInit {
    // Utils for template
    readonly _typeof = (x: any) => typeof x;
    readonly json_stringify = JSON.stringify;

    // State data
    session: Session;
    environment: EnvironmentDefinition;
    scopes: string[] = [];
    generatingToken = false;
    token: string = null;

    constructor(
        private sessionService: SessionService,
        private route: ActivatedRoute,
        private toastr: ToastrService,
    ) {
        this.environment = environment;
    }

    async ngOnInit(): Promise<void> {
        this.route.queryParamMap.subscribe((params: Params) => {
            this.scopes = params.params.scope.split(' ');
        });
    }

    scope_name(scope: string): string {
        return scope
            .replace(/_/g, ' ')
            .replace(/^(\w)/g, (c) => c.toLocaleUpperCase());
    }

    scope_description(scope: string): string {
        if (!(scope in SCOPE_DESCRIPTIONS)) {
            throw Error(`Unknown scope: ${scope}`);
        }
        return SCOPE_DESCRIPTIONS[scope]
    }

    async generateToken() {
        this.generatingToken = true;

        try {
            this.token = (await this.sessionService.generateApiTokenForScopes(this.scopes)).token;
            this.toastr.success('Generation successfull');
        }
        catch (err) {
            let msg = JSON.stringify(err);
            if (msg === '{}') {
                msg = err.message || err.toString();
            }
            this.toastr.error(msg, 'Error generating token');
        }
    }
}
