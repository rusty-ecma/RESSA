const path = require('path');
const fs = require('fs');
const testOutput = `
test angular1                   ... bench: 381,044,294 ns/iter (+/- 8,519,462)
test angular1_min               ... bench: 275,561,324 ns/iter (+/- 8,916,067)
test angular1_min_scanner_only  ... bench: 204,510,797 ns/iter (+/- 4,340,496)
test angular1_scanner_only      ... bench: 277,906,679 ns/iter (+/- 7,690,833)
test jquery                     ... bench: 189,494,434 ns/iter (+/- 14,400,734)
test jquery_min                 ... bench: 149,524,468 ns/iter (+/- 9,639,714)
test jquery_min_scanner_only    ... bench: 111,879,778 ns/iter (+/- 2,272,616)
test jquery_scanner_only        ... bench: 139,757,409 ns/iter (+/- 4,929,847)
test react                      ... bench:  29,444,984 ns/iter (+/- 992,640)
test react_dom                  ... bench: 340,531,355 ns/iter (+/- 7,584,358)
test react_dom_min              ... bench: 146,282,132 ns/iter (+/- 3,214,978)
test react_dom_min_scanner_only ... bench: 108,660,304 ns/iter (+/- 3,117,269)
test react_dom_scanner_only     ... bench: 256,987,200 ns/iter (+/- 5,212,030)
test react_min                  ... bench:  10,493,468 ns/iter (+/- 341,622)
test react_min_scanner_only     ... bench:   7,598,514 ns/iter (+/- 254,301)
test react_scanner_only         ... bench:  21,467,265 ns/iter (+/- 660,417)
test vue                        ... bench: 216,102,694 ns/iter (+/- 5,313,617)
test vue_min                    ... bench: 148,878,598 ns/iter (+/- 4,087,089)
test vue_min_scanner_only       ... bench: 111,318,685 ns/iter (+/- 3,771,835)
test vue_scanner_only           ... bench: 158,757,128 ns/iter (+/- 4,518,811)`;
async function main() {
    let benchOut = await runBench();
    let b = parseOutput(benchOut);
    await addSizes(b);
    let results = reduceScanners(b);
    benchEsprima(results);
    report(results);
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
            scannerOnly: 0,
            esprima: 0,
        }
    }).sort((l, r) => {
        if (l.name > r.name) return 1;
        if (l.name < r.name) return -1;
        return 0;
    });
}

async function addSizes(results) {
    for (let result of results) {
        await addSize(result);
    }
}

function addSize(result) {
    return new Promise((r, j) => {
        let p = paths[result.name.replace('.scanner.only', '')];
        fs.stat(p, (err, stats) => {
            if (err) return j(err);
            result.size = stats.size;
            r();
        });
    });
}

function reduceScanners(results) {
    let scanners = results.filter(r => r.name.indexOf('.scanner.only') > -1).map(r => {
        return {
            name: r.name.replace('.scanner.only', ''),
            ns: r.ns
        }
    });
    let notScanner = results.filter(r => r.name.indexOf('.scanner.only') < 0);
    for (let result of scanners) {

        let item = notScanner.find(r => r.name === result.name)
        item.scannerOnly = result.ns;
    }
    return notScanner;
}

function report(benches) {
    const header = '|test name|bench time|+/-|size|scan only|overhead|esprima|\n';
    const divider = '|---|---|---|---|---|---|---|\n';
    let out = header + divider;
    for (let bench of benches) {
        out += formatEntry(bench);
    }
    let formatted = setColumnWidths(out);
    fs.writeFile('benchmark.md', formatted, err => {
        if (err) throw err;
        console.log(formatted);
    });
}

function formatEntry(entry) {
    const prec = 2;
    let [t, u] = reduceTime(entry.ns);
    let [dt, du] = reduceTime(entry.dev);
    let [s, su] = reduceSize(entry.size);
    let [c, cu] = reduceTime(entry.scannerOnly);
    let [o, ou] = reduceTime(entry.ns - entry.scannerOnly);
    let [e, eu] = reduceTime(entry.esprima);
    let cells = [
        entry.name,
        `${t.toFixed(prec)} ${u}`,
        `${dt.toFixed(prec)} ${du}`,
        `${s.toFixed(prec)} ${su}`,
        `${c.toFixed(prec)} ${cu}`,
        `${o.toFixed(prec)} ${ou}`,
        `${e.toFixed(prec)} ${eu}`,
    ]
    return '|' + cells.join('|') + '|\n'
}

function setColumnWidths(table) {
    let maxWidths = [
        0, // name
        0, // time
        0, // dev
        0, // size
        0, // scan
        0, // oh
        0, //esprima
    ];
    let lines = table.split('\n')
    for (let line of lines) {
        let cells = line.split('|');
        // loop skipping first and last
        for (let i = 1; i < cells.length - 1; i++) {
            let cellWidth = cells[i].trim().length;
            maxWidths[i - 1] = Math.max(maxWidths[i - 1], cellWidth);
        }
    }
    return lines.map((l, i) => {
        return l.split('|').map((c, i) => {
            if (c == '') return c;
            let trimmed = c.trim();
            let pad = trimmed === '---' ? '-' : ' ';

            let requiredPadding = maxWidths[i - 1] - trimmed.length;

            return ' ' + trimmed + pad.repeat(requiredPadding) + ' ';
        }).join('|');
    }).join('\n');
}


function reduceTime(time) {
    let nsInMs = 1000000;
    let msInS = 1000;
    let sInM = 60;
    if (time === undefined) throw new Error('no time provided to reduce');
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

const perf = require('perf_hooks');
const es = require('esprima');
function benchEsprima(results) {
    for (let item of results) {
        console.log('benching ', item.name);
        let p = paths[item.name];
        let js = fs.readFileSync(p).toString();
        let agg = 1000;
        let all = new Array(agg);
        for (let i = 0; i < agg; i++) {
            let start = perf.performance.now();
            es.parseScript(js);
            let end = perf.performance.now();
            all[i] = Math.floor((end - start) * 10000);
        }
        item.esprima = getMedian(all);
    }
}

function getMedian(arr) {
    let sorted = arr.sort((l, r) => l - r);
    if (arr.length % 2 > 0) {
        let middle = Math.floor(sorted.length / 2);
        let middleVal = sorted[middle - 1];
        return (middleVal + sorted[middle]) / 2;
    }
    return sorted[(sorted.length / 2) - 1];
}


main()