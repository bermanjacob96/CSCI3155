/*
	THE PROBLEM:
	Provided a List of integers. We shall only traverse the list once and we
	must return an option of a function from nonthing to int. s.t. if the 
	list contains even numbers we return Some(funciton that finds the
	product of the odd numbers observed in the list) or else we return 
	None and we shouldn't perform any multiplication because the system
	we are running this one is actually like really really bad at
	multiplication. We can however take the output of this and execute it
	on some other computer that does multiplication quickly. Return None for
	the empty list. Return None if there are no odd numbers

	THOUGHTS:
	- what a horid problem. What horrid English... Let's rewrite it

		+ P0
		+ foo: List[Int] => Option[() => Int]
			+ let l be the input list
			+ let li be an element of the list
			+ if there exists an li s.t. li is even
				+ return Some(() => product of all li s.t. li is odd)
			+ else
				+ return None
		+ CONDITION_0: return None if there are no odd numbers
		+ CONDITION_1: only scan the list once
		+ CONDITION_2: Perform no multiplication

	- this is more legible to me, but this seems like a hard problem. I
	  see a similar smaller easier problem. let's solve that first.

		+ P1
		+ foo: List[Int] => Int
			+ let l be the input list
			+ let li be an element of the list
			+ if there exists an li s.t. li is even
				+ return product of all li s.t. li is odd
			+ else
				+ return 1

	- later on I can add back in the constraints to the problem and change
	  the output type back to the expected one as needed. Perhaps here I
	  Note what Ill do. If I get stuck along the way I'll go lookup similar easier
	  problems and solve those before I power forward

	  	+ P1
	  	+ P2 = P1 plus CONDITION_0
	  	+ P3 = P2 plus CONDITION_1
	  	+ P4 = P2 plus CONDITION_2
	  	+ P0 (should be some type reworking from the previous step)

	- Personally I would just keep rewriting the same function from here
		on out, but for legibility I'll be adding suffixes to my funciton
		names to note which one I'm working on
	- I'll use TDD
	- I'll use the testFramework.scala from yesterday's reading
*/

def main() {
	// this is the only function that I will call. I'll call it at the
	// bottom of the file

	testP1()
	testP2()
	testP3()
	testP4()
	// maybe you have a funciton named testP0...
}

def testP1_0() = {
	// Nil
	val t0 = () => {("P1_0_baseCase",(1,() => { P1_0(Nil) }))}
	harnessTestAll(List(t0))
}
def P1_0(l:List[Int]):Int = 1


def testP1_1() = {
	// Nil
	val t0 = () => {("P1_1_baseCase",(1,() => { P1_1(List()) }))}
	// 7::9::Nil
	val t1 = () => {("P1_1_NoEvens",(1,() => { P1_1(List(7,9)) }))}
	harnessTestAll(List(t0,t1))
}
def P1_1(l:List[Int]):Int = 1


def testP1_2() = {
	// Nil
	val t0 = () => {("P1_2_baseCase",(1,() => { P1_2(List()) }))}
	// 7::9::Nil
	val t1 = () => {("P1_2_NoEvens",(1,() => { P1_2(List(7,9)) }))}
	// 2::4::Nil
	val t2 = () => {("P1_2_EvensNoOdds",(1,() => { P1_2(List(7,9)) }))}
	harnessTestAll(List(t0,t1,t2))
}
def P1_2(l:List[Int]):Int = 1


def testP1_3() = {
	// This is useful to make rewriting a bit faster
	val i = 3
	val P1_i = P1_3(_)
	// Nil
	val t0 = () => {(s"P1_${i}_baseCase",(1,() => { P1_i(List()) }))}
	// 7::9::Nil
	val t1 = () => {(s"P1_${i}_NoEvens",(1,() => { P1_i(List(7,9)) }))}
	// 2::4::Nil
	val t2 = () => {(s"P1_${i}_EvensNoOdds",(1,() => { P1_i(List(7,9)) }))}
	// 5::5::2::Nil
	val t3 = () => {(s"P1_${i}_HasBoth",(25,() => { P1_i(List(5,5,2)) }))}
	harnessTestAll(List(t0,t1,t2,t3))
}
def P1_3(l:List[Int]):Int = {
	// subproblem, check if the list has any even values
	// perhaps you write a helper funciton for this, I am
	// fairly comforatable with HOFs so Ill just use exists
	val hasEvens = l exists { li => li % 2 == 0 } // _%2==0
	if (hasEvens) {
		// again, maybe you want a helper, but I'll use foldLeft
		l.foldLeft(1){
			(parialProduct,li) => {
				if (li%2==1) { parialProduct * li } 
				else { parialProduct }
			}
		}
	}
	else { 1 }
}


def testP1_4() = {
	// This is useful to make rewriting a bit faster
	val i = 4
	val P1_i = P1_4(_)
	// Nil
	val t0 = () => {(s"P1_${i}_baseCase",(1,() => { P1_i(List()) }))}
	// 7::9::Nil
	val t1 = () => {(s"P1_${i}_NoEvens",(1,() => { P1_i(List(7,9)) }))}
	// 2::4::Nil
	val t2 = () => {(s"P1_${i}_EvensNoOdds",(1,() => { P1_i(List(7,9)) }))}
	// 5::5::2::Nil
	val t3 = () => {(s"P1_${i}_HasBoth",(25,() => { P1_i(List(5,5,2)) }))}
	// -5::5::2::Nil
	val t4 = () => {(s"P1_${i}_HasBothAndNegs",(-25,() => { P1_i(List(-5,5,2)) }))}
	harnessTestAll(List(t0,t1,t2,t3,t4))
}
def P1_4(l:List[Int]):Int = {
	val hasEvens = l exists { li => li % 2 == 0 } // _%2==0
	if (hasEvens) {
		l.foldLeft(1){
			(parialProduct,li) => {
				// scala WAT!  n%2 == -1 if n is odd and negative
				if (li%2!=0) { parialProduct * li } 
				else { parialProduct }
			}
		}
	}
	else { 1 }
}

def testP1() = {
	// testP1_0()
	// testP1_1()
	// testP1_2()
	// testP1_3()
	// testP1_4()
	// This is useful to make rewriting a bit faster
	val i = ""
	val P1_i = P1(_)
	// Nil
	val t0 = () => {(s"P1_${i}_baseCase",(1,() => { P1_i(List()) }))}
	// 7::9::Nil
	val t1 = () => {(s"P1_${i}_NoEvens",(1,() => { P1_i(List(7,9)) }))}
	// 2::4::Nil
	val t2 = () => {(s"P1_${i}_EvensNoOdds",(1,() => { P1_i(List(7,9)) }))}
	// 5::5::2::Nil
	val t3 = () => {(s"P1_${i}_HasBoth",(25,() => { P1_i(List(5,5,2)) }))}
	// -5::5::2::Nil
	val t4 = () => {(s"P1_${i}_HasBothAndNegs",(-25,() => { P1_i(List(-5,5,2)) }))}
	harnessTestAll(List(t0,t1,t2,t3,t4))
}
// right about here I like to clean up my code, but you might prefer to leave it the
// other way...
def P1(l:List[Int]):Int = {
	if (l exists { _%2 == 0 }){
		l.foldLeft(1){
			(acc,h) => if(h%2!=0) { acc * h } else { acc }
		}
	}
	else { 1 }
}



/*
	Great, we got to see some TDD for the easy problem.
	we wrote it using exists and foldLeft but if we could have written it using
	helper functions instead of the HOFS...

	Moving forward I won't show all my TDD steps...

	CHECKLIST
	  	DONE P1
	  	+ P2 = P1 plus CONDITION_0
	  	+ P3 = P2 plus CONDITION_1
	  	+ P4 = P2 plus CONDITION_2
	  	+ P0 (should be some type reworking from the previous step)


	DISCOVER P2
		+ CONDITION_0: return None if there are no odd numbers
		+ P1
		+ foo: List[Int] => Int
			+ let l be the input list
			+ let li be an element of the list
			+ if there exists an li s.t. li is even
				+ return product of all li s.t. li is odd
			+ else
				+ return 1

	P2
		+ foo: List[Int] => Option[Int]
			+ let l be the input list
			+ let li be an element of the list
			+ if there exists an li s.t. li is even
				+ return product of all li s.t. li is odd
			+ else
				+ return 1
		+ CONDITION_0: return None if there are no odd numbers
		+ EXTRA: Return None if there are no even numbers
*/


def testP2() {
	// Nil - has no evens or odds... return None
	val t0 = () => {("P2_baseCase",(None,() => { P2(List()) }))}
	// 7::9::Nil
	val t1 = () => {("P2_NoEvens",(None,() => { P2(List(7,9)) }))}
	// 2::4::Nil
	val t2 = () => {("P2_EvensNoOdds",(None,() => { P2(List(7,9)) }))}
	// 5::5::2::Nil
	val t3 = () => {("P2_HasBoth",(Some(25),() => { P2(List(5,5,2)) }))}
	// -5::5::2::Nil
	val t4 = () => {(s"P2_HasBothAndNegs",(Some(-25),() => { P2(List(-5,5,2)) }))}
	harnessTestAll(List(t0,t1,t2,t3,t4))
}
def P2(l:List[Int]):Option[Int] = {
	if (l exists { _%2 == 0 }){
		l.foldLeft(None:Option[Int]){
			case (None,h) => if(h%2!=0) { Some(h) } else { None }
			case (acc@Some(parialProduct),h) => if(h%2!=0) { Some(parialProduct*h) } else { acc }
		}
	}
	else { None }
}


/*
	CHECKLIST
	  	DONE P1
	  	DONE P2 = P1 plus CONDITION_0
	  	+ P3 = P2 plus CONDITION_1
	  	+ P4 = P2 plus CONDITION_2
	  	+ P0 (should be some type reworking from the previous step)

	DISCOVER P3
	+ CONDITION_1: only scan the list once
	+ P2
		+ foo: List[Int] => Option[Int]
			+ let l be the input list
			+ let li be an element of the list
			+ if there exists an li s.t. li is even
				+ return product of all li s.t. li is odd
			+ else
				+ return 1
		+ CONDITION_0: return None if there are no odd numbers

	P3
		+ foo: List[Int] => Option[Int]
			+ let l be the input list
			+ let li be an element of the list
			+ if there exists an li s.t. li is even
				+ return product of all li s.t. li is odd
			+ else
				+ return 1
		+ CONDITION_0: return None if there are no odd numbers
		+ EXTRA: Return None if there are no even numbers
		+ CONDITION_1: only scan the list once
*/


def testP3() {
	// Nil - has no evens or odds... return None
	val t0 = () => {("P3_baseCase",(None,() => { P3(List()) }))}
	// 7::9::Nil
	val t1 = () => {("P3_NoEvens",(None,() => { P3(List(7,9)) }))}
	// 2::4::Nil
	val t2 = () => {("P3_EvensNoOdds",(None,() => { P3(List(7,9)) }))}
	// 5::5::2::Nil
	val t3 = () => {("P3_HasBoth",(Some(25),() => { P3(List(5,5,2)) }))}
	// -5::5::2::Nil
	val t4 = () => {(s"P3_HasBothAndNegs",(Some(-25),() => { P3(List(-5,5,2)) }))}
	harnessTestAll(List(t0,t1,t2,t3,t4))
}
def P3(l:List[Int]):Option[Int] = {
	None
	// TODO: implement P3
	// RECALL def of P2
	// def P2(l:List[Int]):Option[Int] = {
	// 	if (l exists { _%2 == 0 }){
	// 		l.foldLeft(None:Option[Int]){
	// 			case (None,h) => if(h%2!=0) { Some(h) } else { None }
	// 			case (acc@Some(parialProduct),h) => if(h%2!=0) { Some(parialProduct*h) } else { acc }
	// 		}
	// 	}
	// 	else { None }
	// }
}


/*
	CHECKLIST
	  	DONE P1
	  	DONE P2 = P1 plus CONDITION_0
	  	DONE P3 = P2 plus CONDITION_1
	  	+ P4 = P2 plus CONDITION_2
	  	+ P0 (should be some type reworking from the previous step)

	DISCOVER P4
	+ CONDITION_2: Perform no multiplication
	+ P3
		+ foo: List[Int] => Option[Int]
			+ let l be the input list
			+ let li be an element of the list
			+ if there exists an li s.t. li is even
				+ return product of all li s.t. li is odd
			+ else
				+ return 1
		+ CONDITION_0: return None if there are no odd numbers
		+ CONDITION_1: only scan the list once

	P4
		TRY IT YOURSLEF... I think we'll need a continuation here...
*/

def testP4() {
	// TODO: implement tests for P4
}
// TODO: Implement P4


/*
	CHECKLIST
	  	DONE P1
	  	DONE P2 = P1 plus CONDITION_0
	  	DONE P3 = P2 plus CONDITION_1
	  	DONE P4 = P2 plus CONDITION_2
	  	+ P0 (should be some type reworking from the previous step)

	+ P0
		+ foo: List[Int] => Option[() => Int]
			+ let l be the input list
			+ let li be an element of the list
			+ if there exists an li s.t. li is even
				+ return Some(() => product of all li s.t. li is odd)
			+ else
				+ return None
		+ CONDITION_0: return None if there are no odd numbers
		+ CONDITION_1: only scan the list once
		+ CONDITION_2: Perform no multiplication
*/

// TODO: Implement P0 and maybe test it...



























// I added the test framework here...
// testFramework.scala

/*
	You don't have to use these. But if you aren't going to write your 
	own test framework I would either use these or go Google around for
	a scala test library that does more of what you want

	If you want to use these funcitons, just plop this file into your other
	files.

	I'll test all your work using the xTestMany function.

	But you'll probably want to test your work using either:
		- harnessTestAll
		- testAll

	Skim this document then move onto noHarenessTestUI.scala

*/




//
// DRIVER
//
def harnessTestAll[A](fDispTests:List[() => (String,(A,()=>A))]):(Int,Int) = {
	val dispTests = fDispTests map { _() }
	testAll(dispTests)
}
def testAll[A](dispTests:List[(String,(A,()=>A))]):(Int,Int) = {
	val (correct, total) = dispTests.foldLeft((0,0)){ case ((correct,total),(testName,test@(expected,fFound))) => {
		
		println(s"TESTING: ${testName}")
		
		val (b, oFound) = xTestOne(test)

		if (b) { dispSuccess() } 
		else { val expected=test._1; dispError(expected, oFound) }

		val totalp = total + 1
		val correctp = correct + Bool2Int(b)
		(correctp, totalp)

	}}

	println("I've finished that work you asked me to do")
	println(s"It seems that out of ${total} tests you succeed in ${correct}")
	// TODO, print some sweet ASCII art if all are correct
	(correct, total)
}

//
// EXECUTES
//
def getExpected[A](fFound:()=>A):Option[A] = {
	/*
		@param fFound: is a function that when executed should yield the 
		found value from execution. But, it might instead throw an error
		I don't want the test framework throwing errors.

		TODO: update to return an Either[Error,A]
			out match { case Left(error) => ???; case Right(found) => ???; }

		@return: Some(found) if fFound returns found:A else None
	*/

	try { Some(fFound()) }
	catch { case _:Throwable => None }
}

def compare[A](expected:A, fFound:()=>A):(Boolean,Option[A]) = {
	/*
		@param expected: the expected value
		@param fFound: the lazy passing of the found expression

		@return: (expected == found, Some(found))
	*/

	getExpected(fFound) match {
		case oFound@Some(found) => (found == expected, oFound)
		case None => (false, None)
	}
}

def xTestOne[A](test:(A,()=>A)):(Boolean,Option[A]) = {
	val (expected, fFound) = test
	compare(expected, fFound)
}

def Bool2Int(b:Boolean):Int = if(b){1}else{0}

def xTestMany[A](tests:List[(A,()=>A)]):(Int,Int) = {
	tests.foldLeft((0,0)){
		case ((correct,total),test) =>{
			val totalp = total + 1
			val correctp = correct + Bool2Int(xTestOne(test)._1)
			(correctp, totalp)
		}
	}
}


//
// DISPLAYS
//
def dispSuccess():Unit = {
	println("    Whose awesome? You're AWESOME!")
	println("")
}

def dispError[A](expected:A, oFound:Option[A]):Unit = oFound match {
	case None => println("    An error occured in testing this work.")
	case Some(found) => {
		println(s"    EXPECTED: ${expected}")
		println(s"    FOUND   : ${found}")
	}
	println("")
}

def dispTestOne[A](dispTest:(String,(A,()=>A))):Unit = {
	val (testName, test) = dispTest
	println(s"TESTING: ${testName}")
	val (b,oFound) = xTestOne(test)
	if (b) { dispSuccess() } 
	else { val expected=test._1; dispError(expected, oFound) }
}

def dispTestMany[A](dispTests:List[(String, (A,()=>A))]):Unit = {
	dispTests foreach dispTestOne
}

def acknowledge():Unit = {
	println("Please hit [ENTER] to continue")
	// TODO: create something that reads User Input
}


main();

