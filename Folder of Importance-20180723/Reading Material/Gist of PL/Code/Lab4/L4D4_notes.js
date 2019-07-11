// L4D4_notes.js

/*
 * Souces:
 * 		https://stackoverflow.com/questions/1295584/most-efficient-way-to-create-a-zero-filled-javascript-array
 */

/*
 *	Lets look at HOF in a language other
 * 	than scala... well use JavaScript
 * 	abbreviated JS
 */

// super useful and standard of JS
const print = function(s){ return console.log(s) }

// JavaScript has arrays... lets write a fold HOF for that
// I won't typescript this... But I'll use decent variable names
// arr : array[A]
// z : B
// cb : (B,A) => B
const foldLeft = function(arr) {
	return function(z) {
		return function(cb) {
			const len = arr.length
			if ( len == 0 ){
				return z
			} else {
				const h = arr[0]
				const t = arr.slice(1)
				return foldLeft(t)(cb(z,h))(cb)
			}
		}
	}
}

const testFoldLeft = function(){
	const arr1 = [5,15,20]
	print(arr1)
	// sum
	print(foldLeft(arr1)(0)((acc,h) => acc + h))
	// times
	print(foldLeft(arr1)(1)((acc,h) => acc * h))
	// printing
	print(foldLeft(arr1)(undefined)((acc,h) => print(h)))
}

testFoldLeft()

const map = function(arr){
	return function(cb) {
		const len = arr.length
		if ( len == 0 ) {
			return arr
		} else {
			const h = arr[0]
			const t = arr.slice(1)
			const hp = cb(h)
			const tp = map(t)(cb)
			return [hp].concat(tp)
		}
	}
}

const testMap = function(){
	const arr1 = [5,15,20]
	print(arr1)
	// increment all val by 2
	print(map(arr1)((h)=>h+2))
	// better yet
	const increment = function (arr,n){
		return map(arr)((h)=>h+n)
	}
	// inc all by 2
	print(increment(arr1,2))
	// inc all by 3
	print(increment(arr1,3))

	// turn all into array of length arri populated with 0s
	// source : https://stackoverflow.com/questions/1295584/most-efficient-way-to-create-a-zero-filled-javascript-array
	print(map(arr1)((h)=> Array(h).fill(0)))
}
testMap()