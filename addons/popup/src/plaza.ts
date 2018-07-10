declare var browser;
declare var chrome;

function init() {
    let entry;
    if (!("browser" in this)) {
        entry = browser;
    } else {
        entry = chrome;
    }
    const Browser = entry;

    function swap_to_signup() {
        (Array.from(document.querySelectorAll("div.login"))
         .forEach((e: HTMLElement) => {
             e.style.display = "none";
         }));

        (Array.from(document.querySelectorAll("div.signup"))
         .forEach((e: HTMLElement) => {
             e.style.display = "block";
         }));

        return false;
    }

    function swap_to_login() {
        (Array.from(document.querySelectorAll("div.signup"))
         .forEach((e: HTMLElement) => {
             e.style.display = "none";
         }));

        (Array.from(document.querySelectorAll("div.login"))
         .forEach((e: HTMLElement) => {
             e.style.display = "block";
         }));

        return false;
    }

    function signup() {
        const username = (document.querySelector('input[name="signup_username"]') as HTMLInputElement).value;
        const password = (document.querySelector('input[name="signup_password"]') as HTMLInputElement).value;
        const passwordCheck = (document
                              .querySelector('input[name="signup_repeat_password"]')as HTMLInputElement)
                              .value;

        if (password !== passwordCheck) {
            return false;
        }

        const payload = { username, password };
        const xmlhttp = new XMLHttpRequest();

        xmlhttp.onreadystatechange = () => {
            if (xmlhttp.readyState === XMLHttpRequest.DONE ) {
                console.log(xmlhttp);
                if (xmlhttp.status === 200) {
                    const success = JSON.parse(xmlhttp.response).success;

                    if (success) {
                        console.log("ok");
                        get_token(username, password);
                    } else {
                        console.log("error");
                    }
                } else if (xmlhttp.status === 400) {
                    console.log("There was an error 400");
                } else {
                    console.log("something else other than 200 was returned: ", xmlhttp.status);
                }
            }
        };

        xmlhttp.open("POST", "https://knowledge-base.hivemind.ai/register");
        xmlhttp.setRequestHeader("Content-Type", "application/json");
        xmlhttp.send(JSON.stringify(payload));

        return false;
    }

    function login() {
        const username = (document.querySelector('input[name="login_username"]') as HTMLInputElement).value;
        const password = (document.querySelector('input[name="login_password"]') as HTMLInputElement).value;

        get_token(username, password);

        return false;
    }

    function get_token(username, password) {
        const payload = { username, password };
        const xmlhttp = new XMLHttpRequest();

        xmlhttp.onreadystatechange = () => {
            if (xmlhttp.readyState === XMLHttpRequest.DONE ) {
                console.log(xmlhttp);
                if (xmlhttp.status === 200) {
                    const success = JSON.parse(xmlhttp.response).success;
                    const token = JSON.parse(xmlhttp.response).token;
                    if (success) {
                        const setting = Browser.storage.local.set({ token });
                        check_token(undefined);
                    } else {
                        console.log("error");
                    }
                } else if (xmlhttp.status === 400) {
                    console.log("There was an error 400");
                } else {
                    console.log("something else other than 200 was returned: ", xmlhttp.status);
                }
            }
        };

        xmlhttp.open("POST", "https://knowledge-base.hivemind.ai/api/get_token");
        xmlhttp.setRequestHeader("Content-Type", "application/json");
        xmlhttp.send(JSON.stringify(payload));
    }

    function check_token(whenToken) {
        const gettingItem = Browser.storage.local.get(["token"], (result) => {
            const token = result.token;

            if (token !== undefined) {
                refresh_link_list_in_ui();
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

                if (whenToken !== undefined) {
                    whenToken();
                }
            } else {
                (Array.from(document
                            .getElementsByClassName("with-token"))
                 .forEach((e: HTMLElement) => {
                     e.style.display = "none";
                 }));
            }
        });
    }

    function silence_with_token() {
        (Array.from(document
                    .getElementsByClassName("with-token"))
         .forEach((e: HTMLElement) => {
             e.style.display = "none";
         }));
    }

    // Logged functions
    function add_site() {
        Browser.tabs.query({active: true, lastFocusedWindow: true}, (tabs) => {
            complete_window_info(tabs[0]);
        });
    }

    function complete_window_info(tab) {
        silence_with_token();

        (Array.from(document
                    .getElementsByClassName("add-site-dialog"))
         .forEach((e: HTMLElement) => {
             e.style.display = "block";

             (e.querySelector('input[name="add-site-title"]') as HTMLInputElement).value = tab.title;
             (e.querySelector('input[name="add-site-url"]') as HTMLInputElement).value = tab.url;
         }));
    }

    function add_site_submit() {
        const title = (document.querySelector('input[name="add-site-title"]') as HTMLInputElement).value;
        const url = (document.querySelector('input[name="add-site-url"]') as HTMLInputElement).value;

        const payload = { title, url };
        const xmlhttp = new XMLHttpRequest();

        xmlhttp.onreadystatechange = () => {
            if (xmlhttp.readyState === XMLHttpRequest.DONE ) {
                console.log(xmlhttp);
                if (xmlhttp.status === 200) {
                    (Array.from(document
                                .getElementsByClassName("add-site-dialog"))
                     .forEach((e: HTMLElement) => {
                         e.style.display = "none";
                     }));

                    check_token(undefined);
                } else if (xmlhttp.status === 400) {
                    console.log("There was an error 400");
                } else {
                    console.log("something else other than 200 was returned: ", xmlhttp.status);
                }
            }
        };

        xmlhttp.open("POST", "https://knowledge-base.hivemind.ai/api/add_site");
        xmlhttp.setRequestHeader("Content-Type", "application/json");

        const gettingItem = Browser.storage.local.get(["token"], (result) => {
            const token = result.token;
            xmlhttp.setRequestHeader("Authorization", "Basic " + btoa(token + ":token"));
            xmlhttp.send(JSON.stringify(payload));
        });

        return false;
    }

    function get_link_data(callback) {
        const xmlhttp = new XMLHttpRequest();

        xmlhttp.onreadystatechange = () => {
            if (xmlhttp.readyState === XMLHttpRequest.DONE ) {
                console.log(xmlhttp);
                if (xmlhttp.status === 200) {
                    (Array.from(document
                                .getElementsByClassName("add-site-dialog"))
                     .forEach((e: HTMLElement) => {
                         e.style.display = "none";
                     }));

                    const response = JSON.parse(xmlhttp.response);
                    callback(response.links);
                } else if (xmlhttp.status === 400) {
                    console.log("There was an error 400");
                } else {
                    console.log("something else other than 200 was returned: ", xmlhttp.status);
                    if (xmlhttp.status === 401) {
                        swap_to_login();
                    }
                }
            }
        };

        xmlhttp.open("GET", "https://knowledge-base.hivemind.ai/api/links");
        xmlhttp.setRequestHeader("Content-Type", "application/json");

        const gettingItem = Browser.storage.local.get(["token"], (result) => {
            const token = result.token;
            if (token !== undefined) {
                xmlhttp.setRequestHeader("Authorization", "Basic " + btoa(token + ":token"));
                xmlhttp.send();
            }
        });
    }

    function save_user_data(data, db) {
        console.log("Data: ", data);
        console.log("DB: ", db);

        const transaction = db.transaction(["links"], "readwrite");
        const linkStore = db.transaction("links", "readwrite").objectStore("links");

        linkStore.clear();
        for (const value of data) {
            linkStore.add(value);
        }

        transaction.oncomplete = (event) => {
            // Store values in the newly created objectStore.
            refresh_link_list_in_ui();
        };
    }

    function synchronize() {
        const request = indexedDB.open("LinksDB", 1);
        request.onupgradeneeded = ((event) => {
            console.log("Upgrade:", event);
            const db = (event.target as any).result;
            const store = db.createObjectStore("links", { autoIncrement: true });
            console.log("Created object store", store);
        });
        request.onerror = (event) => {
            console.error(event);
            alert("Why didn't you allow my web app to use IndexedDB?!");
        };

        request.onsuccess = (event) => {
            const db = (event.target as any).result;
            get_link_data((data) => {
                save_user_data(data, db);
            });
        };
        console.log("REQ:", request);
    }

    function refresh_link_list_in_ui() {
        const request = indexedDB.open("LinksDB", 1);
        request.onupgradeneeded = ((event) => {
            console.log("Upgrade:", event);
            const db = (event.target as any).result;
            const store = db.createObjectStore("links", { autoIncrement: true });
            console.log("Created object store", store);
        });
        request.onsuccess = ((event) => {
            const db = (event.target as any).result;
            const linkStore = db.transaction(["links"], "readonly").objectStore("links");

            const results = [];
            linkStore.openCursor().onsuccess = (cursorEvent) => {
                const cursor = cursorEvent.target.result;
                if (cursor) {
                    results.push(cursor.value);
                    cursor.continue();
                } else {
                    refresh_link_list_in_ui_with_values(results);
                }
            };
        });
    }

    function refresh_link_list_in_ui_with_values(values) {
        const folderMap = {};
        const folders = [];
        for (const value of values) {
            if (!folderMap.hasOwnProperty(value.folder_title)) {
                folderMap[value.folder_title] = {
                    links: [],
                    private: value.folder_private,
                };
                folders.push(value.folder_title);
            }

            const folder = folderMap[value.folder_title];
            folder.links.push(value);
        }

        const topTevel = document.getElementById("knowledge-list");
        topTevel.innerHTML = ""; // @TODO: do this in a more elegant way
        for (const folderName of folders) {
            const title = document.createElement("h1");
            const list = document.createElement("ul");

            list.style.display = "none";
            title.onclick = (() => { toogle_display(list); });
            title.setAttribute("class", "actuable");

            const folder = folderMap[folderName];

            title.innerText = folderName + " (" + folder.links.length + ")";
            for (const link of folder.links) {
                const linkRow = document.createElement("li");
                const linkRef = document.createElement("a");
                linkRef.href = link.link_url;
                linkRef.innerText = link.link_title;

                linkRow.appendChild(linkRef);
                list.appendChild(linkRow);
            }
            topTevel.appendChild(title);
            topTevel.appendChild(list);
        }
    }

    function toogle_display(element) {
        if (element.style.display === "none") {
            element.style.display = "block";
        } else {
            element.style.display = "none";
        }
    }

    // Initialize fields
    (Array.from(document
           .getElementsByClassName("swap-to-signup"))
           .forEach((e: HTMLElement) => {
                e.onclick = swap_to_signup;
     }));

    (Array.from(document
           .getElementsByClassName("swap-to-login"))
           .forEach((e: HTMLElement) => {
                e.onclick = swap_to_login;
     }));

    (Array.from(document
           .getElementsByClassName("signup-button"))
           .forEach((e: HTMLElement) => {
                e.onclick = signup;
     }));

    (Array.from(document
           .getElementsByClassName("login-button"))
           .forEach((e: HTMLElement) => {
                e.onclick = login;
     }));

    (Array.from(document
           .getElementsByClassName("add-site-button-bar"))
           .forEach((e: HTMLElement) => {
                e.onclick = add_site;
     }));

    (Array.from(document
           .getElementsByClassName("add-site-submit-button"))
           .forEach((e: HTMLElement) => {
                e.onclick = add_site_submit;
     }));

    (Array.from(document
           .getElementsByClassName("synchronize-button-bar"))
           .forEach((e: HTMLElement) => {
                e.onclick = synchronize;
     }));

    check_token(synchronize);
}

init();
