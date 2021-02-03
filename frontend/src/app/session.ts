import {User, UserTags} from './user';

export class Session implements User {
    // User interface
    username: string;
    user_id: string;
    tags: UserTags;

    // New elements
    active: boolean;

    constructor(active: boolean, username: string, user_id: string, tags: UserTags) {
        this.active = active;
        this.username = username;
        this.user_id = user_id;
        this.tags = tags || { is_admin : false, is_advanced: false, is_in_preview: false, is_public_profile: false };
    }
}
