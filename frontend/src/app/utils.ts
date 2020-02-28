import { IconReference, HashedIcon } from './connection';
import { ApiRoot } from './api-config';

function toWebsocketUrl(url: string): string {
    if (url.startsWith('/')) { // We need an absolute address for this
        url = document.location.protocol + '//' + document.location.host + url;
    }
    return url.replace(/^http/, 'ws');
}

function iconDataToUrl(icon: IconReference, bridge_id: string): string | undefined {
    if (!icon) { return undefined; }
    if ((icon as {url: string}).url) {
        return (icon as {url: string}).url;
    }
    else if ((icon as HashedIcon).sha256) {
        return ApiRoot + '/assets/icons/' + bridge_id;
    }
}

export {
    toWebsocketUrl,
    iconDataToUrl,
};
