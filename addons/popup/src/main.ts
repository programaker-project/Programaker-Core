import * as PlazaApi from "./PlazaApi";
import * as Storage from "./Storage";

function login() {
    const username = (document.querySelector('input[name="login_username"]') as HTMLInputElement).value;
    const password = (document.querySelector('input[name="login_password"]') as HTMLInputElement).value;

    console.log("Loging in:", username);
    PlazaApi.get_token(username, password)
        .then((token) => Storage.save_auth_token(token))
        .then(() => show_ready());

    return false;
}

function show_ready() {
    // TODO
    console.log("Ready!");
}

function check_token() {
    Storage.get_auth_token().then((token) => {
        console.log("Got token!");
    })
    .catch(() => {
        console.log("No token! :(");
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
