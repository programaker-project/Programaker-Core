declare var window: Window;

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

export function run_on_tab(tab, file: string) {
    Browser.tabs.executeScript(tab.id, {
        file,
    });
}
