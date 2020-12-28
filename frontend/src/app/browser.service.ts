import { Injectable } from '@angular/core';

interface WindowPlaceholder {
    innerHeight?: number,
    innerWidth?: number,
    dispatchEvent: (event: Event) => void,
    localStorage?: Storage,
    sessionStorage?: Storage,
    onresize?: ((this: GlobalEventHandlers, ev: UIEvent) => any) | null,
    onload?: ((this: GlobalEventHandlers, ev: Event) => any) | null,
    open?: (url?: string, target?: string, features?: string, replace?: boolean) => Window | null,
    location?: Location,
};

@Injectable({
  providedIn: 'root'
})
export class BrowserService {

    constructor() {}

    public get window(): Window | WindowPlaceholder {
        // Maybe should use `isPlatformBrowser` instead?
        if ((typeof window) === 'undefined') {
            return {
                dispatchEvent: () => {},
            };
        }
        else {
            return window;
        }
    }

    public get document(): Document | {} {
        // Maybe should use `isPlatformBrowser` instead?
        if ((typeof document) === 'undefined') {
            return {};
        }
        else {
            return document;
        }
    }
}
