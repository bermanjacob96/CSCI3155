// L7Q1.scala

/*
	See lab handout for information.
*/

// def foldRightList[A,B](l:List[A])(z:B)(cb:(A,B)=>B):B = l match{
// 	case Nil => z
// 	case (h :: t) => {
// 	// must look at tail first!
// 	val zp = foldRightList(t)(z)(cb)
// 	val zpp = cb(h,zp)
// 	zpp
// 	}
// }
// def mapList[A,B](l:List[A])(cb:(A)=>B):List[B] = {
// // using fold, need foldRight
// foldRightList(l)(Nil:List[B]){
// 	(h, z) => List(cb(h), z)
// 	}
// }

// Binary Tree
// T ::= Nil | T <- A -> T
// A is a generic language
sealed abstract class BT[+A] // T
case object Empty extends BT // Nil
case class BTNode[A](l:BT[A],d:A,r:BT[A]) extends BT[A] // Tl <- Ad -> Tr

def BTfoldLeft[A,B](t:BT[A])(z:B)(cb:(B,A)=>B):B = t match {
	case Empty => z
	case BTNode(l,d,r) => BTfoldLeft(r)(cb(BTfoldLeft(l)(z)(cb),d))(cb)
}
def BTfoldRight[A,B](t:BT[A])(z:B)(cb:(A,B)=>B):B = t match {
	case Empty => z
	case BTNode(l,d,r) => BTfoldRight(l)(cb(d,BTfoldRight(r)(z)(cb)))(cb)
}
def BTmap[A,B](t:BT[A])(cb:A=>B):BT[B] = t match {
	case Empty => Empty
	case BTNode(l,d,r) => BTNode(BTmap(l)(cb),cb(d),BTmap(r)(cb))
}


// Binned Tree
// B ::= Nil | B <- L -> B
// L ::= Nil | A :: L
type BinT[A] = BT[List[A]]


def testBTLLfoldLeft() = {

	val tree0:BinT[Int] = BTNode(Empty,List(2,3),Empty)
	val tree1:BinT[Int] = BTNode(Empty,List(5,12),BTNode(Empty,List(10,2,7),Empty))

	//val cb0 = { (d:Int) => Nil}
	val t0 = () => { ("FoldLefttest 0", (5, () => SumBinnedTree(tree0)))}
	val t1 = () => { ("FoldLefttest 1", (36, () => SumBinnedTree(tree1)))}


	harnessTestAll(List(t0,t1))


}

def BTLLfoldLeft[A,B](t:BinT[A])(z:B)(cb:(B,A)=>B):B = {
	def foldLeftList[A,B](l:List[A])(z:B)(cb:(B,A)=>B):B = l match{
		case Nil => z
		case h :: t => {
			val zp = cb(z,h)
			val zpp = foldLeftList(t)(zp)(cb)
			zpp
		}
	}
	BTfoldLeft(t)(z){
		(zp, h) => foldLeftList(h)(zp)(cb)
	}
	// def helper(l:List[A])(z:B):B = l match{
	// 	case Nil => z
	// 	case List(h,t) => {
	// 		val zp = cb(z,h)
	// 		val zpp = helper(l)(zp)
	// 		zpp
	// 	}
	// }
	// t match{
	// 	case Empty => z
	// 	case BTNode(l,d,r) => {
	// 		val zp = BTLLfoldLeft(l)(z)(cb)
	// 		val zpp = helper(d)(zp)
	// 		val zppp = BTLLfoldLeft(r)(zpp)(cb)
	// 		zppp
	// 	}
	// }
}

def testBTLLmap() = {
	val tree0:BinT[Int] = BTNode(Empty,List(1), Empty)
	val cb0 =  {(d:Int) => d + 1}
	val t0 = () => { ("Maptest 0", (BTNode(Empty,List(2), Empty), () => BTLLmap(tree0)(cb0)))}

	harnessTestAll(List(t0))
}

def BTLLmap[A,B](t:BinT[A])(cb:A=>B):BinT[B] = t match {
	case Empty => Empty:BinT[B]
	case BTNode(l, d, r) => BTNode(BTLLmap(l)(cb), d.map(cb) ,BTLLmap(r)(cb))
}


def testSumBinnedTree() {
}

def SumBinnedTree(t:BinT[Int]):Int = {
	BTLLfoldLeft(t)(0){ (z,h) => z + h }
}

def existsList[A](l:List[A])(cb: (A)=>Boolean): Boolean =  l match{
	 	case Nil => false
	 	case (h :: t) => cb(h) || existsList(t)(cb)
	//l.foldLeft(l)(false){ (acc, h) => acc || cb(h) }
}

def nonEmptyAndSame[A](l:List[A]):(Option[A],Boolean) = l match {
	case Nil => (None,false)
	case (h :: t) => if ( existsList(t){ _ != h }) (None, false) else (Some(h), true)
	// option TODO
	// a recommended helper function for isBinnedSearchTree
}

def testIsBinnedSearchTree() = {
	// oTODO
}
def isBinnedSearchTree(t:BinT[Int]):Boolean = {
	val ov = BTfoldLeft(t)(Some(Int.MinValue):Option[Int]){
		case (None, _) => None
		case (Some(min),l) => {
			nonEmptyAndSame(l) match {
				case (None, _) => None
				case (Some(d), _) if (d <= min) => None
				case someMinPrime => Some(min)
			}
		}
	}
	ov != None
}


def test() {
	testIsBinnedSearchTree();
	testBTLLmap();
	testSumBinnedTree();
	testBTLLfoldLeft();
}

test



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
	val (correct,total) = testAll(dispTests)

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

	(correct,total)
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




