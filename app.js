'use strict';
const es = require('esprima');
const fs = require('fs');
let js = fs.readFileSync('./examples/simple.js');
function main() {
    let script = es.parseScript(js.toString());
    let text = JSON.stringify(script.body);
    fs.writeFile('out.esprima.json', text, err => {
        if (err) {
            throw err;
        }
        console.log('done');
    })
}

main();