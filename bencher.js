const path = require('path');
const fs = require('fs');
const benches = {
    angular1: {
        normal: {
            ns: 2303806063,
            dev: 629241313,
            path: 'angular/angular.js',
        },
        min: {
            ns: 298483805,
            dev: 133099887,
            path: 'angular/angular.min.js',
        },

    },
    jquery: {
        normal: {
            ns: 512966585,
            dev: 193621991,
            path: 'jquery/dist/jquery.js',
        },
        min: {
            ns: 151366969,
            dev: 38781238,
            path: 'jquery/dist/jquery.min.js',
        },
    },
    react: {
        normal: {
            ns: 38489490,
            dev: 2915616,
            path: 'react/umd/react.development.js',
        },
        min:{
            ns: 10627439,
            dev: 497958,
            path: 'react/umd/react.production.min.js',
        },
    },
    reactDom: {
        normal: {
            ns: 1490179396,
            dev: 672391389,
            path: 'react-dom/umd/react-dom.development.js',
        },
        min:  {
            ns: 157814413,
            dev: 66248993,
            path: 'react-dom/umd/react-dom.production.min.js',
        },
    },
    vue: {
        normal: {
            ns: 633085083,
            dev: 285204631,
            path: 'vue/dist/vue.js'
        },
        min: {
            ns: 154839352,
            dev: 16893259,
            path: 'vue/dist/vue.min.js'
        },
    },
};

function runBench() {
    return new Promise((res, rej) => {
        let bencher = require('child_process').spawn('cargo', ['+nightly', 'bench']);
        let totalOut = '';
        let returned = false;
        bencher.stdout.on('data', (data) => {
            if (typeof data != 'string') data = data.toString();
            totalOut += data;
            console.log('data: ', data);
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

async function main() {
    let benchOut = await runBench();
    console.log('benchOut', benchOut);
}

function report() {
    console.log('| test name | bench time | +/- | size |')
    console.log('|---|---|---|---|');
    for (let k in benches) {
        let test = benches[k];
        let normal = test.normal;
        let normalSize = fs.statSync(path.join(__dirname, 'node_modules', normal.path)).size;
        let normalOut = formatEntry(`${k}.js`, normal.ns, normal.dev, normalSize);
        console.log(normalOut);
        let min = test.min;
        let minSize = fs.statSync(path.join(__dirname, 'node_modules', min.path)).size;
        let minOut = formatEntry(`${k}.min.js`, min.ns, min.dev, minSize);
        console.log(minOut);
    }
}

function formatEntry(name, time, dev, size) {
    let [t, u] = reduceTime(time);
    let [dt, du] = reduceTime(dev);
    let [s, su] = reduceSize(size);
    return `| ${name} | ${t.toFixed(2)} ${u} | ${dt.toFixed(2)} ${du} | ${s.toFixed(2)} ${su} |`;
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