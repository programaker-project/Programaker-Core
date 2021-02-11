declare const NProgress : any;

export function track<T>(p: Promise<T>): Promise<T> {
    NProgress.configure({ showSpinner: false });
    NProgress.start();

    return p.then( x => {
        NProgress.done();
        return x;
    }).
    catch( x => {
        NProgress.done();
        throw x;
    })
}
