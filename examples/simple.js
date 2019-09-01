const cp = require('child_process');
const fs = require('fs');
const prog = require('progress');


function dd(infile, outfile, bytesize) {
    console.log('Getting started');
    var bar;
    var currentBytes;

    fs.stat(infile, function(err, stat) {
        if (err) return console.error('Unable to get infile stats', err.message);
        console.log(`moving \n\t${infile}`);
        console.log(`to \n\t${outfile}`);
        var inFileSize = stat.size;
        bar = new prog('Progress [:bar] :percent :current :total',
        {
            total: inFileSize,
            complete: '‡',
            incomplete: ' '
        });

        var dd = cp.spawn('dd', [`if=${infile}`, `of=${outfile}`, `bs=${bytesize || '1m'}`]);
        var interval = setInterval(function() {
            if (bar.complete) {
                clearInterval(interval)
                console.log('Finishing up');
            } else {
                dd.kill('SIGINFO');
            }
        }, 100);
        dd.addListener('exit', function(code, sig) {
            if (code == 0) {
                bar.tick(bar.total - bar.curr);
                console.log('Complete');
                process.exit();
            } else {
                console.log(`Exit with code ${code}: ${sig}`);
                process.exit();
            }
        });
        // TODO: Add color formatting
        dd.stderr.on('data', function(data) {
            console.log('dd.stderr.on("data", ' + data);
            if (typeof data != 'string') data = data.toString('utf8');
            var status = parse(data);
            var update;
            if (status) {
                update =  status - currentBytes;
                currentBytes = status;
                if (!bar.complete) bar.tick(update);
            }
        });
    });
}

function parse(text) {
    var lines = text.split('\n')
    var line = lines[2]
    if (!line) {
        line = lines[0]
    } 
    var words = line.split(' ')
    return Number.parseInt(words[0])
}

var ifile;
var ofile;
var bs;

if (process.argv[2]) {
    ifile = process.argv[2]
} else {
    console.error('no ifile');
    process.exit();
}
if (process.argv[3]) {
    ofile = process.argv[3]
} else {
    console.error('no ofile');
    process.exit();
}

if (process.argv[4]) {
    bs = process.argv[4]
}

dd(ifile, ofile, bs);

//FIXME nothing used after this
var gen = function*() {
    yield 'one';
    yield 'two';
    yield 'three';
}
let generator = gen();
let current = generator.next();
while (!current.done) {
    console.log('current value:', current.value);
    current = generator.next();
}

var {a, b, c} = {a: 1, b: 2, c: 3};