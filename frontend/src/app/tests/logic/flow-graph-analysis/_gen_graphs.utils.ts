import { spawn } from 'child_process';
import * as fs from 'fs';
import { convert_to_graphviz } from '../scaffolding/utils';

const files = fs.readdirSync(__dirname);
files.forEach((file: string) => {
    if (file.match(/\d+.*.spec.ts$/)) {
        process.stdout.write(`Loading ${file} ... `)

        const mod = require('./' + file.substr(0, file.length - 3)); // Remove '.ts'
        if (mod.gen_flow) {
            fs.writeFileSync(`${__dirname}/${file}.dot`, convert_to_graphviz(mod.gen_flow()));
            spawn("dot", ["-Tpng", `${__dirname}/${file}.dot`, '-o', `${__dirname}/${file}.png`]);
            process.stdout.write('OK\n');
        }
        else {
            process.stdout.write('No flow found\n');
        }
    }
});
