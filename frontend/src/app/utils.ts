import { IconReference, HashedIcon } from './connection';
import { EnvironmentService } from './environment.service';

function toWebsocketUrl(env: EnvironmentService, url: string): string {
    let baseServerPath = document.location.origin;
    const apiHost = env.getBrowserApiHost();
    if (apiHost != '') {
        baseServerPath = apiHost;
    }

    if (url.startsWith('/')) { // We need an absolute address for this
        url = baseServerPath + url;
    }
    return url.replace(/^http/, 'ws');
}


function getUserPictureUrl(env: EnvironmentService, userId: string): string {
    return `${env.getBrowserApiRoot()}/users/by-id/${userId}/picture`;
}

function getGroupPictureUrl(env: EnvironmentService, groupId: string): string {
    return `${env.getBrowserApiRoot()}/groups/by-id/${groupId}/picture`;
}

function iconDataToUrl(env: EnvironmentService, icon: IconReference, bridge_id: string): string | undefined {
    if (!icon) { return undefined; }
    if ((icon as {url: string}).url) {
        return (icon as {url: string}).url;
    }
    else if ((icon as HashedIcon).sha256) {
        return env.getBrowserApiRoot() + '/assets/icons/' + bridge_id;
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

function addTokenQueryString(url: string, token: string): string {
    if (url.indexOf('?') === -1) {
        return url + '?token=' + token;
    }
    else {
        return url + '&token=' + token;
    }
}

function ground(environmentService: EnvironmentService, obj: any, field: string){
    if (obj[field] && obj[field].startsWith('/')) {
        obj[field] = environmentService.getApiRoot() + obj[field];
    }

    return obj;
}

function manageTopLevelError<I,O>(f: (input: I) => O): (input: I) => O {
    return (...args) => {
        try {
            return f(...args);
        }
        catch (error) {
            logError(error);
        }
    };
}

function logError(error: Error & { _logged?: boolean }) {
    if (error._logged) {
        return;
    }

    error._logged = true;

    console.error(error);
}


export {
    toWebsocketUrl,
    getUserPictureUrl,
    getGroupPictureUrl,
    iconDataToUrl,
    unixMsToStr,
    addTokenQueryString,
    ground,

    manageTopLevelError,
    logError,
};
