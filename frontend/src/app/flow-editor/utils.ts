export function uuidv4() {
    // From https://stackoverflow.com/a/2117523
    // Used to generate unique-in-svg IDs for blocks in workspace
    // It just has to be reasonably unique, impredictability here is just overhead.
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}
