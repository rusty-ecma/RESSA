const path = require('path');
const fs = require('fs');
async function main() {
    let benchOut = await runBench();
    let b = parseOutput(benchOut);
    await addSizes(b);
    report(b);
}
const paths = {
    'angular1': path.join(__dirname, 'node_modules', 'angular', 'angular.js'),
    'angular1.min': path.join(__dirname, 'node_modules', 'angular', 'angular.min.js'),
    'jquery': path.join(__dirname, 'node_modules', 'jquery', 'dist', 'jquery.js'),
    'jquery.min': path.join(__dirname, 'node_modules', 'jquery', 'dist', 'jquery.min.js'),
    'react': path.join(__dirname, 'node_modules', 'react', 'umd', 'react.development.js'),
    'react.min': path.join(__dirname, 'node_modules', 'react', 'umd', 'react.production.min.js'),
    'react.dom': path.join(__dirname, 'node_modules', 'react-dom', 'umd', 'react-dom.development.js'),
    'react.dom.min': path.join(__dirname, 'node_modules', 'react-dom', 'umd', 'react-dom.production.min.js'),
    'vue': path.join(__dirname, 'node_modules', 'vue', 'dist', 'vue.js'),
    'vue.min': path.join(__dirname, 'node_modules', 'vue', 'dist', 'vue.min.js'),
}
function runBench() {
    return new Promise((res, rej) => {
        let bencher = require('child_process').spawn('cargo', ['+nightly', 'bench']);
        let totalOut = '';
        let returned = false;
        bencher.stdout.on('data', (data) => {
            process.stdout.write(data);
            if (typeof data != 'string') data = data.toString();
            totalOut += data;
        });
        bencher.stderr.on('data', data => {
            process.stderr.write(data);
        });
        bencher.on('close', (code, sig) => {
            if (!returned) {
                returned = true;
                res(totalOut);
            }
        });
        bencher.on('error', rej);
        bencher.on('exit', (code, sig) => {
            if (!returned) {
                returned = true;
                res(totalOut)

            }
        });
    });
}

function parseOutput(output) {
    let lines = output.split('\n');
    let relevant = lines.filter(l => l.indexOf('... bench:') > -1);
    return relevant.map(l => {
        let parts = l.split(' ').filter(p => p != '');
        let name = parts[1].split('_').join('.');
        let ns = parts[4].replace(/,/g, '');
        ns = parseInt(ns);
        let dev = parts[7];
        dev = parseInt(dev.substring(0, dev.length));
        return {
            name,
            ns,
            dev,
            size: 0,
        }
    })
}

async function addSizes(results) {
    for (let result of results) {
        await addSize(result);
    }
}

function addSize(result) {
    return new Promise((r, j) => {
        let p = paths[result.name];
        fs.stat(p, (err, stats) => {
            if (err) return j(err);
            result.size = stats.size;
            r();
        });
    });
}



function report(benches) {
    const header = '| test name | bench time | +/- | size |\n';
    const divider = '|---|---|---|---|\n';
    for (let bench of benches) {
        let out = formatEntry(bench.name, bench.ns, bench.dev, bench.size)
        let fullOut = header + divider + out;
        fs.writeFile('benchmark.md', fullOut, err => {
            if (err) throw err;
            console.log(fullOut);
        });
    }
}

function formatEntry(name, time, dev, size) {
    let [t, u] = reduceTime(time);
    let [dt, du] = reduceTime(dev);
    let [s, su] = reduceSize(size);
    return `| ${name}.js | ${t.toFixed(2)} ${u} | ${dt.toFixed(2)} ${du} | ${s.toFixed(2)} ${su} |`;
}


function reduceTime(time) {
    let nsInMs = 1000000;
    let msInS = 1000;
    let sInM = 60;
    if (time >= nsInMs * msInS * sInM * sInM) {
        return [time / nsInMs / msInS / sInM / sInM, 'h'];
    }
    if (time >= nsInMs * msInS * sInM) {
        return [time / nsInMs / msInS / sInM, 'm'];
    }
    if (time >= nsInMs * msInS) {
        return [time / nsInMs / msInS, 's'];
    }
    if (time >= nsInMs) {
        return [time / nsInMs, 'ms']
    }
    return [time, 'ns']
}

function reduceSize(size) {
    let factor = 1024;
    //            kb        mb
    if (size >= factor * factor) {
        return [size / factor / factor, 'mb'];
    }
    if (size >= factor) {
        return [size / factor, 'kb'];
    }
    return [size, 'b'];
}
main()