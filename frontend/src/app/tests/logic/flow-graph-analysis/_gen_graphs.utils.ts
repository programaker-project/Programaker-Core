import { spawn } from 'child_process';
import * as fs from 'fs';
import { convert_to_graphviz } from '../scaffolding/utils';
import * as util from 'util';

export function run(): Promise<any[]> {
    const files = fs.readdirSync(__dirname);
    const candidates = files.filter((file: string) => file.match(/\d+.*.spec.ts$/));

    let done_count = 0;
    const promises = candidates.map((file: string) => {
        return new Promise(async (resolve, reject) => {
            try {
                const mod_name = file.substr(0, file.length - 3);
                const mod = require('./' + mod_name); // Remove '.ts'
                if (mod.gen_flow) {
                    await util.promisify(fs.writeFile)(`${__dirname}/${file}.dot`, convert_to_graphviz(mod.gen_flow()));
                    spawn("dot", ["-Tpng", `${__dirname}/${file}.dot`, '-o', `${__dirname}/${file}.png`]);

                    if (mod.process_flow) {
                        await util.promisify(fs.writeFile)(`${__dirname}/${file}.processed.dot`,
                                                           convert_to_graphviz(mod.process_flow(mod.gen_flow())));
                        spawn("dot", ["-Tpng", `${__dirname}/${file}.processed.dot`, '-o', `${__dirname}/${file}.processed.png`]);
                    }

                    done_count++;
                    process.stdout.write(`[${done_count}/${candidates.length}] ${file}\n`)
                    resolve(file);
                }
                else {
                    process.stdout.write('No flow found\n');
                    resolve(null);
                }
            }
            catch (err) {
                process.stdout.write(file + ': ' + err.toString() + '\n');
                reject(err);
            }
        });
    });

    return Promise.all(promises) as Promise<any[]>;
}
