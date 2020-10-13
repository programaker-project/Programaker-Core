const API_ROOT = "http://127.0.0.1:8888/api/v0";

export function get_token(username: string, password: string): Promise<string> {
    return new Promise((resolve, reject) => {
        const payload = { username, password };
        const xmlhttp = new XMLHttpRequest();

        xmlhttp.onreadystatechange = () => {
            if (xmlhttp.readyState === XMLHttpRequest.DONE ) {
                console.log(xmlhttp);
                if (xmlhttp.status === 200) {
                    const success = JSON.parse(xmlhttp.response).success;
                    const token = JSON.parse(xmlhttp.response).token;
                    if (success) {
                        console.log("Got TOKEN!", token);
                        resolve(token);
                    } else {
                        reject("No success");
                    }
                } else if (xmlhttp.status === 400) {
                    console.log("There was an error 400");
                    reject("Error code 400");
                } else {
                    console.log("something else other than 200 was returned: ", xmlhttp.status);
                    reject("something else other than 200 was returned: " + xmlhttp.status);
                }
            }
        };

        xmlhttp.open("POST", API_ROOT + "/sessions/login");
        xmlhttp.setRequestHeader("Content-Type", "application/json");
        xmlhttp.send(JSON.stringify(payload));
    });
}

export function send_xpath_monitor(username: string, token: string, payload: any): Promise<any> {
    return new Promise((resolve, reject) => {
        const xmlhttp = new XMLHttpRequest();

        xmlhttp.onreadystatechange = () => {
            if (xmlhttp.readyState === XMLHttpRequest.DONE ) {
                if (xmlhttp.status === 200) {
                    resolve(JSON.parse(xmlhttp.response));
                } else if (xmlhttp.status === 400) {
                    reject({
                        message: "Error code 400",
                        status: xmlhttp.status,
                    });
                }
            } else {
                console.log("something else other than 200 was returned: ", xmlhttp.status);
                reject({
                    message: "something else other than 200 was returned: " + xmlhttp.status,
                    status: xmlhttp.status,
                });
            }
        };

        xmlhttp.open("POST", API_ROOT + "/users/" + username + "/monitors/");
        xmlhttp.setRequestHeader("Content-Type", "application/json");
        xmlhttp.setRequestHeader("Authorization", token);
        xmlhttp.send(JSON.stringify(payload));
    });
}
