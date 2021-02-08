// The file contents for the current environment will overwrite these during build.
// The build system defaults to the dev environment which uses `environment.ts`, but if you do
// `ng build --env=prod` then `environment.prod.ts` will be used instead.
// The list of which env maps to which file can be found in `.angular-cli.json`.

import { EnvironmentDefinition } from "./environment-definition";

export const environment : EnvironmentDefinition = {
    contact_mail: 'configure-this@on-environment.com',
    production: false,
    ApiHost: 'http://localhost:8888',
    aboutPageRender: 'https://programaker.com/api/v0/programs/by-id/5fad1d5e-9f69-4f7f-93db-cbb6816d8b22/render/',
    SSRAboutPageRender: null,
};
