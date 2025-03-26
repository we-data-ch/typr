export function map<T, U>(array: T[], callback: (value: T, index: number, array: T[]) => U): U[] {
    const result: U[] = [];
    for (let i = 0; i < array.length; i++) {
        result.push(callback(array[i], i, array));
    }
    return result;
}

export function numeric_add(a: number, b: number): number {
	return a + b;
}

export function integer_add(a: number, b: number): number {
	return a + b;
}

export function numeric_minus(a: number, b: number): number {
	return a - b;
}

export function integer_minus(a: number, b: number): number {
	return a - b;
}

export function numeric_mul(a: number, b: number): number {
	return a * b;
}

export function integer_mul(a: number, b: number): number {
	return a * b;
}

export function numeric_div(a: number, b: number): number {
	return a / b;
}

export function integer_div(a: number, b: number): number {
	return a / b;
}

export function integer_seq(start: number, count: number, step: number = 1): number[] {
	return [...Array(count)].map((_, i) => start + i * step);
}

