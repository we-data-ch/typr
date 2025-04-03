export function to_string(value: any): string {
    if (typeof value === 'number') {
        return value.toString();
    } else if (typeof value === 'string') {
        return value;
    } else if (typeof value === 'boolean') {
        return value.toString();
    } else if (value === null) {
        return 'null';
    } else if (value === undefined) {
        return 'undefined';
    } else if (Array.isArray(value)) {
        return '[' + value.map(to_string).join(', ') + ']';
    } else if (typeof value === 'object') {
        let result = '{';
        let first = true;
        for (let key in value) {
            if (value.hasOwnProperty(key)) {
                if (!first) {
                    result += ', ';
                }
                result += key + ': ' + to_string(value[key]);
                first = false;
            }
        }
        return result + '}';
    } else {
        return 'unknown type';
    }
}
