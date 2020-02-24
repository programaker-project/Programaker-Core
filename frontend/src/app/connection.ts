export type HashedIcon = { sha256: string };

export type IconReference = null | { url: string } | HashedIcon;

export interface BridgeConnection {
    connection_id: string,
    name: string,
    icon: IconReference,
    bridge_id: string,
    bridge_name: string,
};
