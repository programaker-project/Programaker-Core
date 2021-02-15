import { CollaboratorRole } from "./types/collaborator";

export interface GroupInfo {
    name: string,
    canonical_name: string,
    id: string,
    picture?: string,
    'public': boolean,
    min_level_for_private_bridge_usage: CollaboratorRole | 'not_allowed',
};

export interface UserGroupInfo extends GroupInfo {
    role: CollaboratorRole,
};
