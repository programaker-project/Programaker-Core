export interface UserTags {
    is_admin: boolean;
    is_advanced: boolean;
    is_in_preview: boolean;
}

export interface User {
    username: string;
    user_id: string;
    tags: UserTags;
}
