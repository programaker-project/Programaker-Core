import { IconReference, HashedIcon } from './connection';
import { ApiRoot } from './api-config';

function toWebsocketUrl(url: string): string {
    if (url.startsWith('/')) { // We need an absolute address for this
        url = document.location.protocol + '//' + document.location.host + url;
    }
    return url.replace(/^http/, 'ws');
}

function getUserPictureUrl(userId: string): string {
    return `${ApiRoot}/users/by-id/${userId}/picture`;
}

function getGroupPictureUrl(groupId: string): string {
    return `${ApiRoot}/groups/by-id/${groupId}/picture`;
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

function unixMsToStr(ms_timestamp: number, options?: { ms_precision?: boolean }): string {
    const date = new Date(ms_timestamp);

    if (!options) {
        options = {};
    }

    const left_pad = ((val: string | number, target_length: number, pad_character: string) => {
        let str = val.toString();

        while (str.length < target_length) {
            str = pad_character + str;
        }
        return str;
    });
    const pad02 = (val: string|number) => {
        return left_pad(val, 2, '0');
    }

    let result = (`${date.getFullYear()}/${pad02(date.getMonth() + 1)}/${pad02(date.getDate())} `
                  + ` - ${pad02(date.getHours())}:${pad02(date.getMinutes())}:${pad02(date.getSeconds())}`);

    if (options.ms_precision) {
        result += `.${date.getMilliseconds()}`;
    }
    return result;
}

export {
    toWebsocketUrl,
    getUserPictureUrl,
    getGroupPictureUrl,
    iconDataToUrl,
    unixMsToStr,
};
