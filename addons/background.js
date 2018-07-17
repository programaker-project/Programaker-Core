"use strict";

const API_ROOT = "https://plaza.spiral.systems/api/v0";

function send_xpath_monitor(username, token, payload) {
    return new Promise((resolve, reject) => {
        const xmlhttp = new XMLHttpRequest();

        xmlhttp.onreadystatechange = () => {
            if (xmlhttp.readyState === XMLHttpRequest.DONE ) {
                if (xmlhttp.status === 200) {
                    const success = JSON.parse(xmlhttp.response).success;
                    const token = JSON.parse(xmlhttp.response).token;
                    if (success) {
                        resolve(token);
                    } else {
                        reject({
                            status: xmlhttp.status,
                            message: "No success"
                        });
                    }
                } else if (xmlhttp.status === 400) {
                    reject({
                        status: xmlhttp.status,
                        message: "Error code 400"
                    });
            } else {
                console.log("something else other than 200 was returned: ", xmlhttp.status);
                reject({
                    status: xmlhttp.status,
                    message: "something else other than 200 was returned: " + xmlhttp.status
                });
            }
        };

        xmlhttp.open("POST", API_ROOT + "/users/" + username + "/monitors/");
        xmlhttp.setRequestHeader("Content-Type", "application/json");
        xmlhttp.setRequestHeader("Authorization", token);
        xmlhttp.send(JSON.stringify(payload));
    })
};


browser.runtime.onMessage.addListener(function(message, sender, sendResponse) {
    if (message.command === 'addMonitor') {
        const token = message.token;
        const payload = message.message;
        const username = message.username;

        send_xpath_monitor(username, token, payload);
    };
});