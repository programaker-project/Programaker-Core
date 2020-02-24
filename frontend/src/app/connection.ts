export type HashedIcon = { sha256: string };

export type IconReference = null | { url: string } | HashedIcon;

export interface BridgeConnection {
    connection_id: string,
    name: string,
    icon_url?: string,
    bridge_id: string,
    bridge_name: string,
};
