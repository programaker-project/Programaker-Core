import { Area2D } from "./flow_block";

export function uuidv4() {
    // From https://stackoverflow.com/a/2117523
    // Used to generate unique-in-svg IDs for blocks in workspace
    // It just has to be reasonably unique, impredictability here is just overhead.
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

export function isContainedIn(contained: Area2D, container: Area2D): boolean {
    const diffX = contained.x - container.x ;
    const diffY = contained.y - container.y;

    return ((diffX >= 0)
        && (diffY >= 0)
        && ((container.width - diffX) >= contained.width)
        && ((container.height - diffY) >= contained.height)
           );
}

export function maxKey<T>(list: T[], key: (el: T) => number): T | null {
    let max = null;
    let maxEl = null;
    for (const el of list) {
        const num = key(el);

        if ((max == null) || (num > max)) {
            max = num;
            maxEl = el;
        }
    }

    return maxEl;
}
