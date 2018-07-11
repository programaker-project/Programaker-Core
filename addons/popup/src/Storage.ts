const DB_NAME = "PlazaDB";
const DB_VERSION = 1;
const AUTH_TOKEN_STORE = "auth_token";

function get_db(): Promise<IDBDatabase> {
    return new Promise((resolve, reject) => {
        const request = indexedDB.open(DB_NAME, DB_VERSION);

        request.onupgradeneeded = ((event) => {
            console.log("Upgrade:", event);
            const db = (event.target as any).result;
            const store = db.createObjectStore(AUTH_TOKEN_STORE, { autoIncrement: true });
            console.log("Created object store", store);
        });

        request.onerror = (event) => {
            console.error(event);
            reject(event);
        };

        request.onsuccess = (event) => {
            const db = (event.target as any).result;
            resolve(db);
        };

        console.log("REQ:", request);
    });
}

export function save_auth_token(token: string): Promise<void> {
    return get_db().then((db) => {
        return new Promise<void>((resolve) => {
            const transaction = db.transaction([AUTH_TOKEN_STORE], "readwrite");
            const storage = transaction.objectStore(AUTH_TOKEN_STORE);

            storage.add(token);

            console.log("Saving");
            transaction.oncomplete = (e) => {
                console.log("Saved!");
                resolve();
            };
        });
    });
}

export function get_auth_token(): Promise<string> {
    return get_db().then((db) => {
        return new Promise<string>((resolve, reject) => {
            try {
                const transaction = db.transaction([AUTH_TOKEN_STORE], "readonly");
                const storage = transaction.objectStore(AUTH_TOKEN_STORE);

                let resolved = false;
                storage.openCursor().onsuccess = (cursorEvent) => {
                    const cursor = (cursorEvent.target as any).result as IDBCursorWithValue;

                    if (cursor) {
                        if (!resolved) {
                            resolve(cursor.value);
                        }
                        resolved = true;
                        cursor.continue();
                    } else {
                        if (!resolved) {
                            reject("No entries");
                        }
                    }
                };
            } catch (e) {
                reject(e);
            }
        });
    });
}
