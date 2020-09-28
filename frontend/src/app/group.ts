import { CollaboratorRole } from "./types/collaborator";

export interface GroupInfo {
    name: string,
    canonical_name: string,
    id: string,
    picture?: string,
    'public': boolean,
};

export interface UserGroupInfo extends GroupInfo {
    role: CollaboratorRole,
};
