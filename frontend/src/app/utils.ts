function toWebsocketUrl(url: string): string {
    if (url.startsWith('/')) { // We need an absolute address for this
        url = document.location.protocol + '//' + document.location.host + url;
    }
    return url.replace(/^http/, 'ws');
}

export {
    toWebsocketUrl
};
