export class Session {
    username: string;
    active: boolean;
    user_id: string;

    constructor(active: boolean, username: string, user_id: string) {
        this.active = active;
        this.username = username;
        this.user_id = user_id;
    }
}
