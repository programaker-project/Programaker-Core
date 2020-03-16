export interface SessionTags {
    is_admin: boolean;
    is_advanced: boolean;
    is_in_preview: boolean;
}

export class Session {
    username: string;
    active: boolean;
    user_id: string;
    tags: SessionTags;

    constructor(active: boolean, username: string, user_id: string, tags: SessionTags) {
        this.active = active;
        this.username = username;
        this.user_id = user_id;
        this.tags = tags;
    }
}
