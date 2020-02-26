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
