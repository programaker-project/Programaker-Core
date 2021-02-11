export type EnvironmentDefinition = {
    contact_mail: string | null,
    production: boolean,
    ApiHost: string,
    aboutPageRender?: string | null,
    SSRApiHost?: string | null,
    SSRAboutPageRender?: string | null,
    YjsWsSyncServer?: string | null,
};
