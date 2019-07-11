// L3D3_notes.js


/*
	NOTE : I am not strictly enforcing my type restrictions.
	I am simply stating the desired types as comments
	(because I want this to be in lab 3 javascripty)

	I am also requiring that all of my functions take exactly 1
	input because this is required for javascripty. This is not
	a requirement of JavaScript
 */
// let e1:(Undefined) => Number
// Plus : ((Undefined)=>Number,(Undefined)=>Number) => Number
function plus(e1) {
	const out = e1(undefined) + e1(undefined)
	return out
}

// f : (Undefined) => Number
function f(x) {
	return 1 + 2 * 3
}

// f: (Undefined) => Number
// a mostly useless line because functions are weird to print
console.log(f) 

// f(_:Undefined) : Number
// prints 7
console.log(f(undefined))

// plus(_:(Undefined) => Number ) : Number
// prints 14
console.log(plus(f))

// throws an error
// console.log(plus(f(undefined)))

// g : (Undefined) => Number
function g(x) {
	return ("15"*true) || console.log("hi")
}

// g : (Undefined) => Number
// a mostly useless line
console.log(g) 

// g(_:Undefined) : Number
// prints 15
console.log(g(undefined))

// plus(_:(Undefined) => Number ) : Number
// prints 30
console.log(plus(g))

// throws error
// console.log(plus(g(undefined)))

/*
	To provide you a taste of what our
	interpreter can accomplish... consider
	the following curried funciton declaration
	and use.

	I will talk about this more later in lab 3
 */

// e1 : (Undefined) => Number
// outer : ((Undefined) => Number) => ((Undefined) => Number) => Number
function outer(e1) {
	// e2 : (Undefined) => Number
	// inner : ((Undefined) => Number) => Number
	function inner(e2) {
		return e1() + e2()
	}
	return inner
}

// outer : ((Undefined) => Number) => ((Undefined) => Number) => Number
// doesn't print much of anything
console.log(outer)

// outer(_:(Undefined) => Number)) : ((Undefined) => Number) => Number
// doesn't print much of anything
console.log(outer(f))

// outer(_:(Undefined) => Number)(_:(Undefined)=>Number) : Number
// prints 22
console.log(outer(f)(g))