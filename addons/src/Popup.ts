import * as BrowserApi from "./BrowserApi";
import * as ProgramakerApi from "./ProgramakerApi";
import * as Storage from "./Storage";

function login() {
    const username = (document.querySelector('input[name="login_username"]') as HTMLInputElement).value;
    const password = (document.querySelector('input[name="login_password"]') as HTMLInputElement).value;

    console.log("Loging in:", username);
    ProgramakerApi.get_token(username, password)
        .then((token) => Storage.save_auth_token(username, token))
        .then(() => show_ready());

    return false;
}

function show_ready() {
    (Array.from(document
        .getElementsByClassName("no-token"))
        .forEach((e: HTMLElement) => {
            e.style.display = "none";
    }));

    (Array.from(document
        .getElementsByClassName("with-token"))
        .forEach((e: HTMLElement) => {
            e.style.display = "block";
    }));
}

function show_login() {
    (Array.from(document
        .getElementsByClassName("no-token"))
        .forEach((e: HTMLElement) => {
            e.style.display = "block";
    }));

    (Array.from(document
        .getElementsByClassName("with-token"))
        .forEach((e: HTMLElement) => {
            e.style.display = "none";
    }));
}

function check_token() {
    Storage.get_auth_token().then(([username, token]) => {
        show_ready();
        BrowserApi.get_current_tab()
            .then((tab) => {
                BrowserApi.run_on_tab(tab, "/popup/injected.js", () => {
                    BrowserApi.send_message_to_tab(tab, {programakerInjectedOptions: { username, token }});
                    BrowserApi.close_popup();
                });
            }, (error) => {
                console.error("Error requesting tab:", error);
            });
    })
    .catch(() => {
        show_login();
    });
}

function prepare() {
    // Initialize fields
    (Array.from(document
            .getElementsByClassName("login-button"))
            .forEach((e: HTMLElement) => {
                e.onclick = login;
        }));

    check_token();
}

prepare();
