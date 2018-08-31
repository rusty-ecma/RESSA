

function Thing() {
    try {
        Stuff()
    } catch (message) {
        console.log(message)
    }
}

function Stuff() {
    throw new Error('jahahahhahahaha')
}

function Third(one, two) {
    return one + two + 3 + 4 + 10;
}

Thing();

let other = (one, two) => {
    return 3;
}