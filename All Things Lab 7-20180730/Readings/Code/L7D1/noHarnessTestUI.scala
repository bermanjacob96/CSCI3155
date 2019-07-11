// L7Q2.scala

/*
	This exectues testGetEvens using the testAll function

	it looks at how we could use the testAll function while testing

	You might not bother reading this one. But you might find it informative.

	When you're done here, move onto harnessTestUI.scala
*/

// getEvens
def getEvens(l:List[Int]):List[Int] = Nil


def testGetEvens() = {
	/*
		I need to write some tests, I'll use TDD
		starting with the base case then moving forward
	*/

	// T1
	val t1Name = "getEvens Should return the empty list on an empty input"
	// A is List[Int]
	val l1:List[Int] = Nil
	val expected1:List[Int] = Nil
	def fFound1():List[Int] = {
		getEvens(l1)
	}
	// scala has (IMO) a strange feature
	/*
		consider a val x:() => A. A use site of x will
		actually call x even if you don't use the () to call.

		A way around this is to state the type

		so since fFound1 has type () => List[Int] I needed
		to declare it's type.

		alternativly I could have written out it's type explicitely
	*/
	val t1:(List[Int],()=>List[Int]) = (expected1,fFound1)
	// I'm passing this case ! yeah!


	// T2
	val t2Name = "getEvens Should return the empty list on a List of odd numbers"
	// A is List[Int]
	val l2:List[Int] = List(1,3,7,5,19)
	val expected2:List[Int] = Nil
	def fFound2():List[Int] = {
		getEvens(l2)
	}
	val t2:(List[Int],()=>List[Int]) = (expected2,fFound2)
	// I'm passing this case ! yeah!


	// T3
	val t3Name = "getEvens Should return the original list on a List of even numbers"
	// A is List[Int]
	val l3:List[Int] = List(2,4,8,6,2018)
	val expected3:List[Int] = List(2,4,8,6,2018)
	def fFound3():List[Int] = {
		getEvens(l3)
	}
	val t3:(List[Int],()=>List[Int]) = (expected3,fFound3)
	// :( I'm failing this test, I need to refine the difinition of 
	// getEvens before I move forward

	// TODO More tests

	val getEvensDispTests:List[(String,(List[Int],()=>List[Int]))] = List(
			(t1Name, t1),
			(t2Name, t2),
			(t3Name, t3)
		)
	println("\n\n\nBEGIN TEST: testGetEvens ==============================================\n\n")
	testAll[List[Int]](getEvensDispTests)
	acknowledge()
	println("\n\n\nEND TEST: testGetEvens ==============================================\n\n")
}






//
// DRIVER
//
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
