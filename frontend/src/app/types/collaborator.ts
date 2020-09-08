export type CollaboratorRole = 'admin' | 'editor' | 'viewer';

export interface Collaborator {
    id: string;
    username: string;
    picture?: string;
    role: CollaboratorRole
};
