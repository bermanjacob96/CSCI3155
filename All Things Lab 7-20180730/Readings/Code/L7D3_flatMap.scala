// L7D3_flatMap.scala

/*
	I even took the time to test that this works  

		^ ^
		0.0
		 O

	We canceled Lab 5 this semester so that we could spend more time on HOFs and
	continuation passing. I'm happy we made this decision but I thought we should
	cover something about Monads...

	VIDEOS:
	 - WATCH this VIDEO: https://www.youtube.com/watch?v=t1e8gqXLbsU&t=911s
	 - maybe watch this one too: https://www.youtube.com/watch?v=9QveBbn7t_c
	 - And if you really want to you might watch this one as well (but I wouldn't because its too technical for my taste): https://www.youtube.com/watch?v=ZhuHCtR3xq8&t=3119s

	I am not an expert on monads. If you ask me about them I'd probably just
	tell you a lot of the same things in those videos. But my understanding
	is that:

		- monads are functions with some cool properties
		- scala collections (List[A], Map[A,B], Option[A]...) are monads
			- https://stackoverflow.com/questions/35698485/how-is-list-a-monad
		- monads allow us to create a flatMap of a for yeild statement
	
	So with that in mind. Let's try to use flatMap. Here is a reading to consider
	- http://www.brunton-spall.co.uk/post/2011/12/02/map-map-and-flatmap-in-scala/
*/



/*
	PROBLEM:
	given a list of integers, find each 3 in the list and return 2 * that element and
	put that in the output list. The output list shall be maintain the order of the 
	elements observed. You may only scan the list once.

	THOUGHTS:
	seems like a SUPER impractical problem. and it is... this has so many constraints
	that don't make a lot of sense. Why not just cound the number of 3s in the list
	and then construct a list of 6s that is of length count of 3s? Two reasons, one
	because I said so... stop thinking... no one is paying you to think... But the better
	reason is that while this isn't a practical problem solving this problem might help us
	solve more practical problems that have these constraints of
	- itterate over a list once
	- take some of the items and apply a transformation to them
	- take the transformed items and create the new list that is a sublist of the original
		 and maintain the order of elements found

	you know... hypothetically if we ever needed to solve a problem like this one...
	or perhaps a simpler version of it...

	FOO:
	 + foo:(List[Int]) => List[Int]
	 + let l be the input list
	 + let li be an element of l
	 + if li is 3 then append 6 to the output list	
	 + else pass the output list forward
*/

// fist Ill solve this directly
def fooDirect(l:List[Int]):List[Int] = {
	def loop(lIn:List[Int],lOut:List[Int]):List[Int] = lIn match {
		case Nil => lOut
		case h :: t if (h == 3) => {
			val hp = h * 2
			loop(t, lOut:::List(hp))
		}
		case _ :: t => loop(t, lOut)
	}
	loop(l,Nil)
}

// then I'll be all like, OH that looks like foldLeft
def fooFoldLeft(l:List[Int]):List[Int] = {
	l.foldLeft(Nil:List[Int]){
		(lOutPartial, li) => {
			if (li == 3) { lOutPartial ::: List(li*2) }
			else { lOutPartial }
		}
	}
}


// then I'll be all like, foldRight would be better for this problem
def fooFoldRight(l:List[Int]):List[Int] = {
	l.foldRight(Nil:List[Int]){
		(li, lOutPartial) => {
			if (li == 3) { li*2 :: lOutPartial }
			else { lOutPartial }
		}
	}
}


// But then I'd be all like, I wonder if there is a better HOF for the problem
/*
	HOFs that I know
	- fold
	- foldLeft
	- foldRight
	- map
	- exists
	- find
	- forall
	- foreach
	- flatMap

	The task at hand is sort of mapping. I could try to use map...
*/

def fooMapTake1(l:List[Int]):List[Option[Int]] = {
	l map {
		li => if (li == 3) Some(li*2) else None
	}
}

/*
	that is pretty clean to read but it doesn't really solve the problem... I'll need to 
	go back through the list
*/

def fooMapTake2(l:List[Int]):List[Int] = {
	val lOI = l map {
		li => if (li == 3) Some(li*2) else None
	}
	lOI.foldRight(Nil:List[Int]){
		case (None, acc) => acc
		case (Some(lOI_i),acc) => lOI_i :: acc
	}
}

/*
	and now it's more correct but that seems like a lot of code.... thinking about all
	the HOF we know of and how they work flatMap is sort of a mystery at this point.
	But it's actaully really effective at solving problems like this. Heres a solution
	maybe you can figure out what it actually does
*/

def fooFlatMap(l:List[Int]):List[Int] = {
	l flatMap {
		li => if (li == 3) List(li*2) else Nil
	}
}


def dispRes(lvl:Int, correct:Int, total:Int):Unit = lvl match { 
	case 1 => {
		if (correct == total) {
			println("\t\t^ ^")
			println("\t\t- -")
			println("\t\t .")
			println("\t\t     _$")
			println("\t\t( )-'")
			println("\t\t. .")
		}
		else {
			println("\t\tYou could do better...")
		}
	}
	case 2 => {
		if (correct == total) {
			println("\t$$")
		}
		else {
			println("\tXXXXXXXXXXXXXX")
		}
	}
}
def flatMap_harness_xTestMany[A](fTests:List[() => (A,()=>A)]):(Int,Int) = {
	val tests = fTests map { _() }
	val (correct, total) = xTestMany(tests)
	dispRes(2,correct,total)
	(correct,total)
}
def testByThing(testName:String, thing:(List[Int])=>List[Int]):(Int,Int) = {
	println(testName)
	// Nil => Nil
	val t1 = () => { (List(), () => thing(List())) }
	// 3::Nil => 6::Nil
	val t2 = () => { (List(6), () => thing(List(3))) }
	// 1::Nil => Nil
	val t3 = () => { (List(), () => thing(List(1))) }
	// 1::3::2::3::4::3::5 => 6::6::6::Nil
	val t4 = () => { (List(6,6,6), () => thing(List(1,3,2,3,4,3,5))) }
	flatMap_harness_xTestMany(List(t1,t2,t3,t4))
}
def testing() {
	val l = List(
			("direct",fooDirect(_)),
			("fold left",fooFoldLeft(_)),
			("fold right", fooFoldRight(_)),
			// (fooMapTake1)  // has type issues...
			("map and fold left", fooMapTake2(_)),
			("flat map", fooFlatMap(_)),
		)
	val (correct,total) = l.foldLeft((0,0)){ case ((correcti,totali),(testNamei,thingi)) =>
		val (ci,ti) = testByThing(testNamei,thingi)
		(correcti + ci, totali + ti)
	}
	// I'll call this at the bottom of the file
	

	println("\n\n\n")
	dispRes(1,correct,total)
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






























testing();

