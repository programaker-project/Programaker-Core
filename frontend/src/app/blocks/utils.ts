export function alreadyRegisteredException(e: Error): boolean {
    return e.message.match(/Error: Extension .* is already registered./) !== null;
}

export function createDom(type: string, attributes?: { [key: string]: string }): HTMLElement {
    const element = (goog.dom as any).createDom(type) as HTMLElement;

    if (attributes) {
        for (const key of Object.keys(attributes)) {
            element.setAttribute(key, attributes[key]);
        }
    }

    return element;
}