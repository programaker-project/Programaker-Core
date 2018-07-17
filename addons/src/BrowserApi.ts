declare const window: Window;
declare const Components;

let entry;
if (("browser" in window)) {
    entry = (window as any).browser;
} else {
    entry = (window as any).chrome;
}

export const Browser = entry;

export function get_current_tab(): Promise<any> {
    return new Promise((resolve, reject) => {
        Browser.tabs.query({active: true, lastFocusedWindow: true}, (tab) => {
            if (tab === undefined) {
                reject("Error retrieving tab");
            }
            resolve(tab[0]);
        });
    });
}

export function run_on_tab(tab, file: string, cb: () => void) {
    Browser.tabs.executeScript(tab.id, {
        file,
    }, () => {
        console.log("CB", cb);
        if (cb) {
            cb();
        }
    });
}

export function send_message_to_tab(tab, data: any) {
    Browser.tabs.sendMessage(tab.id, data);
}

export function close_popup() {
    window.close();
}
