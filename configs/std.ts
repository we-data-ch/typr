export function typr<A>(a: A): void {
	console.log("Hello TypR!")
}

export function as__character<A>(value: A): string {
    if (value === null) return "null";
    if (value === undefined) return "undefined";
    if (typeof value === "object") return JSON.stringify(value);
    return String(value);
}

export function console__log<A>(value: A): void {
	console.log(value)
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
