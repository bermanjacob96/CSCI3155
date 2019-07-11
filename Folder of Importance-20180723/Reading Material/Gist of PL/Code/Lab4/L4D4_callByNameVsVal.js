/* 	I appologize for not posting these notes sooner
	It seems that I completely forgot to write notes
	on call by name vs call by value

	In Lab 4 we interoduce modes into our expressions

		Grammar level
			m ::= const | name

		AST level
			sealed abstract class Mode    		// m
			case object MConst extends Mode 	// const
			case object MName extends Mode 		// name

	const is just like it was in lab 3. in binding some
	expression to a variable, I must reduce the expression
	to a value before binding the variable. NOTE that our 
	small step interpret performs binding via substitution

	name is a bit special... it represents a concept called
	lazy evaluation. If an expression is bound to a variable
	by name (rather than by const) then the expression is
	bound to the variable as is. i.e. the expression is bound
	regardless of whether or not it is a value

	This logic about whether something is or is not reducable
	should be handled in the isRedex function.

	Please note that this is not valid in JavaScript but it
	is valid javascripty. This concept is something that 
	actually exists in some langauges, commonly using the
	key term 'lazy' rather than the key term 'name'
 */

 /* EXAMPLE 1
 	m x = (console.log("hi"),(1 + 2)) ; x + x
 	what if m is const? what if it is name?
  */
	/* EXAMPLE 1 CONST */
		const x = (console.log("hi"),(1 + 2)) ; x + x
		// -> SearchDecl, SearchSeq, DoPrint # hi
		const x = (undefined,(1 + 2)) ; x + x
		// -> SearchDecl, DoSeq
		const x = 1 + 2 ; x + x
		// -> SearchDecl, DoArith
		const x = 3 ; x + x
		// -> DoDecl
		3 + 3 
		// -> 
		6 // Q.E.D.
		// note it prints hi one time
		// it took 5 steps

	/* EXAMPLE 1 Name */
		name x = (console.log("hi"),(1 + 2)) ; x + x
		// -> DoDecl
		(console.log("hi"),(1 + 2)) + (console.log("hi"),(1 + 2))
		// -> SearchBinary1, SearchSeq, DoPrint # hi
		(undefined,(1 + 2)) + (console.log("hi"),(1 + 2))
		// -> SearchBinary1, DoSeq
		((1 + 2)) + (console.log("hi"),(1 + 2))
		// -> SearchBinary1, DoArith(Plus)
		3 + (console.log("hi"),(1 + 2))
		// -> SearchBinary2, SearchSeq, DoPrint # hi
		3 + (undefined,(1 + 2))
		// -> SearchBinary2, DoSeq
		3 + ( 1 + 2 )
		// -> SearchBinary2, DoArith(Plus)
		3 + 3
		// -> DoArith(Plus)
		6 // Q.E.D.
		// note it prints hi twice
		// it took 8 steps

	/* 	EXAMPLE 1 TAKEAWAY
		if a variable is used multiple times it will definitely
		be more efficient to bind using the mode const

		using the mode name can have unnexpected side effects
		like overprinting
	 */


 /* EXAMPLE 2
 	m x = (console.log("hi"),(1 + 2)) ; 5
 	what if m is const? what if it is name?
  */
	/* EXAMPLE 2 CONST */
		const x = (console.log("hi"),(1 + 2)) ; 5
		// -> SearchDecl, SearchSeq, DoPrint # hi
		const x = (undefined,(1 + 2)) ; 5
		// -> SearchDecl, DoSeq
		const x = 1 + 2 ; 5
		// -> SearchDecl, DoArith
		const x = 3 ; 5
		// -> DoDecl
		5 // Q.E.D.
		// note it prints hi one time
		// it took 4 steps

	/* EXAMPLE 2 Name */
		name x = (console.log("hi"),(1 + 2)) ; 5
		// -> DoDecl
		5 // Q.E.D.
		// note it prints nothing
		// it took 1 step

	/* 	EXAMPLE 2 TAKEAWAY
		if a variable is never used then it is more efficient
		to bind the variable my name but this might cause
		for a lack of side effects such as a lack of printing
	 */

/*	in adition to being used for declarations/variable binding
	sites, modes are used at call sights. For this reason we
	often describe this concept with call-by-name vs call-by-value.

	As you probably guessed :
		call-by-name works with the mode name
		call-by-value works with the mode const

	In lab 4 we allow each of our parameters to have a specified mode
 */

/* 	EXAMPLE 3
	Where this whole concept becomes rather interesting
	is when we look at expressions that might or might not
	actually use the bound variable

	Consider the function (from lab 3 javascripty)
	function ( x , y ) {
		( x ) ? y : 3
	}

		Now consider potential calls to that function
			function ( x , y ) {( x ) ? y : 3} ( true , 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 )
				this will observe the value of y during execution
			function ( x , y ) {( x ) ? y : 3} ( false , 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 )
				this will NOT observe the value of y during execution

	So It might make sense, if I were programming in a langauge
	that allows for lazy evaluation, to pass the parameter y by 
	its name rather than its value
 */

/* EXAMPLE 3.1 */
	function ( x : const bool , y : name number ) {
		( x ) ? y : 3
	}( false , 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 )
	// -> DoCall
	( false ) ?  1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9  : 3
	// -> DoIfFalse
	3 // Q.E.D.
	// evaluates in 2 steps

/* EXAMPLE 3.2 */
	function ( x : const bool , y : name number ) {
		( x ) ? y : 3
	}( true , 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 )
	// -> DoCall
	( true ) ?  1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9  : 3
	// -> DoIfTrue
	1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9
	// -> SearchBinary1, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	2 * 3 * 4 * 5 * 6 * 7 * 8 * 9
	// -> SearchBinary1, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	6 * 4 * 5 * 6 * 7 * 8 * 9
	// -> SearchBinary1, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	24 * 5 * 6 * 7 * 8 * 9
	// -> SearchBinary1, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	120 * 6 * 7 * 8 * 9
	// -> SearchBinary1, SearchBinary1, SearchBinary1, DoArith(Times)
	720 * 7 * 8 * 9
	// -> SearchBinary1, SearchBinary1, DoArith(Times)
	5040 * 8 * 9
	// -> SearchBinary1, DoArith(Times)
	40320 * 9
	// ->  DoArith(Times)
	362880

/* 	TAKEAWAY
	variable on the value of x, if y is bound by name
	than the function call might* evaluate quickyl
 */
/* EXAMPLE 3.3 */
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 6 * 4 * 5 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 24 * 5 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 120 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 720 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 5040 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 40320 * 9 )
	// -> SearchCall2, DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( false , 362880 )
	// -> DoCall
	( false ) ? 362880 : 3
	// -> DoIfFalse 
	362880

/* EXAMPLE 3.4 */
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 6 * 4 * 5 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 24 * 5 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 120 * 6 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, ..., DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 720 * 7 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, SearchBinary1, DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 5040 * 8 * 9 )
	// -> SearchCall2, SearchBinary1, DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 40320 * 9 )
	// -> SearchCall2, DoArith(Times)
	function ( x : const bool , y : const number ) {
		( x ) ? y : 3
	}( true , 362880 )
	// -> DoCall
	( true ) ? 362880 : 3
	// -> DoIfTrue
	362880

/*	TAKEAWAY
	variable on the value of x, if y is bound by const
	than the function call will always take quite a while
	to evaluate.
 */

/* 	TAKEAWAY On the whole

	There are some expressions that we as programmers will
	write in our livetime in which we will wish our PL had 
	the option to pass values lazily / by name. Sure enough
	some langauges do allow us this oportunity. Some languages
	will do it in slightly different ways... like pass it by
	name BUT if you EVER evaluate it, save that value found and
	use it the next time that you see it. We won't cover that
	in this course but it is an interesting thought exercise.
 */