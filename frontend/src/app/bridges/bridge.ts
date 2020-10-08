import { IconReference } from '../connection';

export interface BridgeMetadata {
    control_url: string;
};

export interface BridgeIndexData {
    owner: string;
    name: string;
    id: string;
    service_id: string;
    is_connected: boolean;
    icon: IconReference;
}

export type BridgeResourceMap = {[key: string]: {[key: string]: string}[]};


export interface FullOwnerId {
    type: 'group' | 'user',
    id: string,
};

export interface SharedResource {
    bridge_id: string,
    icon: IconReference,
    name: string,
    resource: string,
    value_id: string,
    shared_by: FullOwnerId
};

export interface BridgeResourceEntry{
    name: string,
    id: string,
    connection_id: string,
    shared_with?: FullOwnerId[],
};
export type BridgeResource = { name: string, values: BridgeResourceEntry[] };

export type BridgeSignal = any;

export interface BridgeTokenInfo {
    name: string,
};

export interface FullBridgeTokenInfo extends BridgeTokenInfo {
    key: string,
};
