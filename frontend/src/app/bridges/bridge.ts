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

export interface BridgeResourceEntry{
    name: string,
    id: string,
    shared_with?: {type: 'group' | 'user', id: string}[],
};
export type BridgeResource = { name: string, values: BridgeResourceEntry[] };

export type BridgeSignal = any;
