import { Browser } from "./BrowserApi";
import * as ProgramakerApi from "./ProgramakerApi";

Browser.runtime.onMessage.addListener((message, sender, sendResponse) => {
    if (message.command === "addMonitor") {
        const token = message.token;
        const payload = message.message;
        const username = message.username;

        ProgramakerApi.send_xpath_monitor(username, token, payload);
    }
});
