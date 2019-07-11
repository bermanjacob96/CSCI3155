// L4D3_notes.scala

// 
// This document is mostly designed to be skimmed as opposed to read.
// 

/*
 - Author : Spencer Wilson
 - Date : 07/03/2017
 - comment : This is not the most amazing thing ever...
 			 But I hope this is enough for you to play with
 			 and learn a little. And continue to unravle
 			 the mystery of computation.
 */

/* S ::= S <- d -> S | End */
sealed abstract class BinaryTree[+A] // S
case object Empty extends BinaryTree // End
case class Node[A](l:BinaryTree[A],d:A, r:BinaryTree[A]) extends BinaryTree[A] // S<-d->S or rather Sl <- d -> Sr


def foldRight[A,B](S:BinaryTree[A])(z:B)(cb : ( A , B ) => B ) : B = S match {
	/*Rule1

		----------------------------------
			z = foldRight( End )(z)(cb)
	 */
	case Empty => z	
	/*Rule2
			zp = foldLeft(SRight )(z)(cb)     	 zpp = cb(d,zp)           zppp = foldRight(SLeft)(zpp)(cb)
		-----------------------------------------------------------------------------------------------------
			zppp = foldLeft( SLeft <- d -> SRight )(z)(cb)
	 */
	case Node(l,d,r) => foldRight(l)(cb(d,foldRight(r)(z)(cb)))(cb)
	// {
	// 	val zp = foldRight(r)(z)(cb)
	// 	val zpp = cb(d,zp)
	// 	val zppp = foldRight(l)(zpp)(cb)
	// }
}

// Here is the map over trees...
def map[A,B](t:BinaryTree[A])(cb:(A)=>B):BinaryTree[B] = t match {
	/*Rule1

		--------------------------------------
			End = map(End)(cb)
	 */
	case Empty => Empty
	/*Rule2
		Sleft' = map(Sleft)(cb)  	Sright' = map(Sright)(cb)	 d' = cb(d) 	  S' = Sleft' <- d' -> Sright'
		--------------------------------------------------------------------------------------------------------
			S' = map(Sleft<-d->Sright)(cb)
	*/
	case Node(l,d,r) => Node(map(l)(cb),cb(d),map(r)(cb))
	// {
	// 	val lp = map(l)(cb)
	// 	val rp = map(r)(cb)
	// 	val dp = cb(d)
	// 	val sp = Node(lp,dp,rp)
	// 	sp
	// }

}

// and one more... exists
// I haven't tested this... But I am fairly confident it works
// certainly not the most elegant solution
def exists[A](t:BinaryTree[A],goal:A):Boolean = t match {
	/*Rule1

		---------------------------------
			false = exists(End,goal)
	 */
	case Empty => false
	/*Rule2
				d == goal
		---------------------------------------
			true = exists(Sl<-d->Sr,goal)
	 */
	case Node(l,d,r) if (d == goal) => true
	/*Rule3
		d != goal   b = exists(Sl,goal) || exists(Sr,goal)
	 	-----------------------------------------------------
	 		b = exists(Sl<-d->Sr,goal)
	 */
	case Node(l,d,r) => exists(l,goal)||exists(r,goal)
}

/*
 * ------------------------------------------------------------------
 *
 *      TESTING SPACE
 *
 * ------------------------------------------------------------------
 */
testAll()

def testAll() = {
	testFold()
	testMap() // Found after all of the folding tests
}


/*
 *
 *	TESTING FOLD
 *
 */
def testFold() = {
	println("\nTESTING FOLD ------------------------")
	testSum()
	testAscending()
	testingFoldNature()
}
/*	A great first thing to test... something simple
	such as summation
 */
def testSum() = {
	println("\n\tBegin Sum Tests ---------------------------")
	val bt : BinaryTree[Int] = Node(Node(Empty,20,Empty),25,Node(Empty,30,Empty))
	def myCallBack( d:Int , z:Int ):Int = { d + z } 
	val myInitAcc = 0
	val sumOfbt = foldRight(bt)(myInitAcc)(myCallBack)
	println("\t\tthe sume of "+bt+" was found to be: "+sumOfbt)

	val bt2:BinaryTree[Int] = Node(bt,5,Node(Empty,0,Empty))
	println("\t\tthe sume of "+bt2+" was found to be: "+foldRight(bt2)(myInitAcc)(myCallBack))

	println("")
}
/*	But Perhaps I want to do something more interesting...
	Perhaps I want to know if the tree is in accending order
	here my call back and initial condition are a bit harder
	to figure out
 */
def testAscending() = {
	println("\n\tBegin Ascending Tests ---------------------------")
	val bt : BinaryTree[Int] = Node(Node(Empty,20,Empty),25,Node(Empty,30,Empty))
	val bt2:BinaryTree[Int] = Node(bt,5,Node(Empty,0,Empty))
	// bt is in acending order
	// bt2 is not in acsending order
	// So I'll use asserts to see if my work is alright

	/* 	This callback must be able to determine
		if the tree is ordered base on the value
		last observed and the current value being
		observed. Note that the tree is being 
		observed from Right (highest value) to Left
		(lowest value)

		But I need to return a boolean... for this reason
		the z needs to either be a tuple of (_:Boolean,_:Int)
		OR I can use the Option[Int] data type... Ill do both 
		for the heck of it
	 */
	def myNewCallBackTupleStyle(d:Int,z:(Boolean,Int)):(Boolean,Int) = {
		if ( z._1 ) (d<=z._2,d) else (false,d) // doesn't actually matter once the thing is false...
	}
	/*	Best accomplished by assuming innocent until proven guilty.
		Here I have the highest possible value for the tree as the 
		initial value to observe
	 */
	val myInitAcc = (true,Int.MaxValue)

	assert( foldRight(bt)(myInitAcc)(myNewCallBackTupleStyle)._1 )
	println("\t\tthis tree is in acending order :"+bt)
	assert( !foldRight(bt2)(myInitAcc)(myNewCallBackTupleStyle)._1 )
	// println("\t\tthis tree is NOT in acending order :"+bt2)

	// Now with Options and a wrapping function...
	def isAcsending(t:BinaryTree[Int]):Boolean = {
		def myNewCallBackOptionStyle(d:Int,z:Option[Int]):Option[Int] = {
			z match {
				case None => z
				case Some(prevData) => if (d <= prevData) Some(d) else None
			}
		}
		val myInitAccOptionStyle : Option[Int] = Some(Int.MaxValue)

		val oi = foldRight(t)(myInitAccOptionStyle)(myNewCallBackOptionStyle)

		val res = oi match {
			case None => false
			case Some(_) => true
		}

		res
	}

	assert( isAcsending(bt) )
	println("\t\tthis tree is in acending order :"+bt)
	assert( !isAcsending(bt2) )
	// println("\t\tthis tree is NOT in acending order :"+bt2)

	println("\t\tAll assertions passed") // unreachable code if the assertions fail
	println("")
}
/* 	And Of course... I should make sure that I am actually folding
	from the right to the left... To do this I'll need to observe
	side effects... I'll do the easy one... Printing
 */
def testingFoldNature() = {
	println("\ttestingFoldNature --------------------------------")

	val bt : BinaryTree[Int] = Node(Node(Empty,20,Empty),25,Node(Empty,30,Empty))
	val bt2:BinaryTree[Int] = Node(bt,5,Node(Empty,0,Empty))

	println("\t\tfolding Right over: "+bt)
	println("\t\tI expect 30\\n25\\n20\\n")
	println("================================")
	foldRight(bt)(())((d,_)=>println(d))
	println("================================")
	println("")
	println("\t\tfolding Right over: "+bt2)
	println("\t\tI expect 0\\n5\\n30\\n25\\n20\\n")
	println("================================")
	foldRight(bt2)(())((d,_)=>println(d))
	println("================================")
	println("")
}

/*
 *
 *	TESTING Map
 *
 */
// these tests are written to show you more flavors for testing
def testMap() = {
	println("\nTESTING MAP ---------------------------------")

	val bt : BinaryTree[Int] = Node(Node(Empty,20,Empty),25,Node(Empty,30,Empty))
	val bt2:BinaryTree[Int] = Node(bt,5,Node(Empty,0,Empty))

	describeTests("test_Increment")
	test_Increment(bt,2)
	test_Increment(bt2,5)

	val bt3:BinaryTree[String] = Node(Node(Empty,"hello",Empty),"2",Node(Empty,"",Empty))
	val bt4:BinaryTree[String] = Node(Empty,"-56",bt3)

	describeTests("test_toDouble")
	test_toDouble(bt3)
	test_toDouble(bt4)
}
def test_Increment(t:BinaryTree[Int],n:Int):Unit = {
	println("\t\tattempting to increment the tree: ")
	printTree(t)
	println("\t\tby the value: "+n)
	def increment(t:BinaryTree[Int],n:Int):BinaryTree[Int] = {
		map(t)((d) => d + n )
	}
	val tp = increment(t,n)
	println("\t\tWe found the tree :")
	printTree(tp)
	println("")
}
def test_toDouble(t:BinaryTree[String]):Unit = {
	println("\t\tattempting to turn this tree into to a tree of integers: ")
	printTree(t)

	// source : https://stackoverflow.com/questions/19386964/i-want-to-get-the-type-of-a-variable-at-runtime
	def typeOf[T: Manifest](t: T): Manifest[T] = manifest[T]

	def toDoubleTree(t:BinaryTree[String]):BinaryTree[Double] = {
		map(t)((d) => try { d.toDouble } catch { case _:Throwable => Double.NaN } )		
	}

	println("\t\tHere is the tree I found :")
	val tp = toDoubleTree(t)
	printTree(tp)

	println()
}
def printTree[A](t:BinaryTree[A]):Unit = {
	println("\t\t\tTree("+foldRight(t)("")((d,s_acc)=> d+","+s_acc)+")")
}
def describeTests(s:String):Unit = {
	println("\n\t"+s+"-----------------------\n")
}
