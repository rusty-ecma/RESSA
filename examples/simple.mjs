export default class Thing {
    constructor(a = 'a', b = 'b', c = 'c') {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    get d() {
        let c_type = typeof this.c;
        if (c_type == 'string') {
            let _c = this.c.charCodeAt(0);
            return String.fromCharCode(_c + 1);
        } else if (c_type == 'number') {
            return c + 1;
        } else {
            throw new Error('Unable to compute d');
        }
    }
}

export function main() {
    let x = new Thing();
    return x.d;
}

console.log(main());