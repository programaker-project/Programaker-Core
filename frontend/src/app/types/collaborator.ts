export type CollaboratorRole = 'admin' | 'editor' | 'viewer';

export function roleToIcon(role: CollaboratorRole) {
    switch(role) {
        case 'admin':
            return 'flash_on';
        case 'editor':
            return 'brush';
        case 'viewer':
            return 'visibility';
        default:
            return null;
    }
}

export interface Collaborator {
    id: string;
    username: string;
    picture?: string;
    role: CollaboratorRole
};
