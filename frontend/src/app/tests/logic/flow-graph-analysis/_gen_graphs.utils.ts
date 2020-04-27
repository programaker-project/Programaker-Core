import { spawn } from 'child_process';
import * as fs from 'fs';
import { convert_to_graphviz } from '../scaffolding/utils';
import * as util from 'util';

export function run(): Promise<any[]> {
    const files = fs.readdirSync(__dirname);

    const promises = files
        .filter((file: string) => file.match(/\d+.*.spec.ts$/))
        .map((file: string) => {
            return new Promise(async (resolve, reject) => {
                const mod_name = file.substr(0, file.length - 3);
                const mod = require('./' + mod_name); // Remove '.ts'
                if (mod.gen_flow) {
                    try {
                        await util.promisify(fs.writeFile)(`${__dirname}/${file}.dot`, convert_to_graphviz(mod.gen_flow()));
                        spawn("dot", ["-Tpng", `${__dirname}/${file}.dot`, '-o', `${__dirname}/${file}.png`]);

                        if (mod.process_flow) {
                            await util.promisify(fs.writeFile)(`${__dirname}/${file}.processed.dot`,
                                                               convert_to_graphviz(mod.process_flow(mod.gen_flow())));
                            spawn("dot", ["-Tpng", `${__dirname}/${file}.processed.dot`, '-o', `${__dirname}/${file}.processed.png`]);
                        }

                        process.stdout.write(`${file} OK\n`)
                        resolve(file);
                    }
                    catch (err) {
                        process.stdout.write(file + ': ' + err.toString() + '\n');
                        reject(err);
                    }
                }
                else {
                    process.stdout.write('No flow found\n');
                    resolve(null);
                }
            });
        });

    return Promise.all(promises) as Promise<any[]>;
}
