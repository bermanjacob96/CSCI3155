// L7Q2.scala

/*
	HOFS

	See the lab 7 handout for specifications of what your functions should do. And
	how they should be defined.

	This is a working space for solving the problems. Do it however you'd like.
	Recommendations for this shall be provided in your reading assignments.
*/

/*

	SRC

 */
def getByPolicy[A](l:List[A])(policy:(A)=>Boolean):List[A] = {
	l flatMap{ (h => if (policy(h)) List(h) else Nil) }
	}

def getEvens(l:List[Int]):List[Int] = {
	l flatMap {
		h => if (h%2 == 0) { List(h) /* h::Nil */ } else { Nil /* List() */ }
	}
}

def getNonNegs(l:List[Int]):List[Int] = {
	l flatMap {
		h => if (h > 0) { List(h) /* h::Nil */ } else { Nil /* List() */ }
	}
}

def getConsonants(l:List[Char]):List[Char] = {
	l flatMap {
		//h => if (h != 'a' || 'e' || 'i' || 'o' || 'u') { List(h) /* h::Nil */ } else { Nil /* List() */ }
		h => h match {
			case 'a' => Nil
			case 'e' => Nil
			case 'i' => Nil
			case 'o' => Nil
			case 'u' => Nil
			case _ => List(h)
		}		
	}
}

/*

	TESTS

 */
def testGetEvens() = {
	// oTODO
}
def testGetNonNegs() = {
	// oTODO

}
def testGetConsonants() = {
	val t0 = () => ("test a::a::b", (List('b'), () => {getConsonants(List('a','a','b'))}))
	val t1 = () => ("other test", (List('x','y','z'), () => {getConsonants(List('a','a','x','e','y','a','a','z'))}))


	harnessTestAll(List(t0, t1))
}
def testGetByPolicy() = {
	// Personally I wouln't test this directly...
	// we can test it by testing the other functions...
	testGetEvens()
	testGetNonNegs()
	testGetConsonants()
}
def test() = {
	testGetByPolicy()
}


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




test()