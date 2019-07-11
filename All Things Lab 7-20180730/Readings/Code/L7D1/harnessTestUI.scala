// L7Q2.scala

/*
	This exectues testGetEvens using the harnessTestAll function

	The previous system required too much work on my end to change 1s to 2s
	and then 2s to threes.

	So I added a wrapper/harness to testAll that makes the work a bit 
	easier for me

	I also added a t4 test that is written (IMO) in a more clean fashion
*/

// getEvens
def getEvens(l:List[Int]):List[Int] = Nil


def testGetEvens() = {

	def t1():(String,(List[Int],()=>List[Int])) = {
		val tName = "getEvens Should return the empty list on an empty input"
		// A is List[Int]
		val l:List[Int] = Nil
		val expected:List[Int] = Nil
		def fFound():List[Int] = {
			getEvens(l)
		}
		val t:(List[Int],()=>List[Int]) = (expected,fFound)

		(tName,t)
	}

	def t2():(String,(List[Int],()=>List[Int])) = {
		val tName = "getEvens Should return the empty list on a List of odd numbers"
		// A is List[Int]
		val l:List[Int] = List(1,3,7,5,19)
		val expected:List[Int] = Nil
		def fFound():List[Int] = {
			getEvens(l)
		}
		val t:(List[Int],()=>List[Int]) = (expected,fFound)

		(tName,t)
	}
	

	def t3():(String,(List[Int],()=>List[Int])) = {
		val tName = "getEvens Should return the original list on a List of even numbers"
		// A is List[Int]
		val l:List[Int] = List(2,4,8,6,2018)
		val expected:List[Int] = List(2,4,8,6,2018)
		def fFound():List[Int] = {
			getEvens(l)
		}
		val t:(List[Int],()=>List[Int]) = (expected,fFound)

		(tName,t)
	}

	def t4():(String,(List[Int],()=>List[Int])) = {
		// List(1,2,3,4,5)
		(	"getEvens should return sublist of odd numbers in t4",	// testName
			(	List(1,3,5),									   	// expected
				() => {												// fFound
					getEvens(List(1,2,3,4,5))
				}
			)
		)
	}
	// TODO More tests
	
	println("\n\n\nBEGIN TEST: testGetEvens ==============================================\n\n")
	val getEvensDispTests:List[() => (String, (List[Int],()=>List[Int]))] = {
		List(t1,t2,t3,t4) // add more tests ...
	}
	harnessTestAll[List[Int]](getEvensDispTests)
	acknowledge()
	println("\n\n\nEND TEST: testGetEvens ==============================================\n\n")
}


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


testGetEvens
