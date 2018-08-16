

function Thing() {
    try {
        Stuff()
    } catch (message}) {
        console.log(message)
    }
}

function Stuff() {
    throw new Error('jahahahhahahaha')
}

Thing()