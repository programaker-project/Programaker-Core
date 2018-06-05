export class Session {
    username: string;
    active: boolean;

    constructor(active: boolean, username: string) {
        this.active = active;
        this.username = username;
    }
}
