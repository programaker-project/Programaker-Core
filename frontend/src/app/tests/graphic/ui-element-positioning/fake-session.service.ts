import { Session } from "app/session";
import { Injectable } from "@angular/core";

@Injectable()
export class FakeSessionService /* implements SessionService */ {
    constructor() {}

    getSession(): Promise<Session> {
        return Promise.resolve({
            username: 'fake-username',
            user_id: '123-fake',
            active: true,
            tags: {
                is_admin: false,
                is_advanced: false,
                is_in_preview: false,
            }
        });
    }
}
