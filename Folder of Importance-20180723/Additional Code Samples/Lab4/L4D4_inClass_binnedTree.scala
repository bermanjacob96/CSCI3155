// L4D4_inClass_binnedTree.scala

/*
	@Author: Spencer D. Wilson
	@Date: 07/08/2018

	Use as you see fit, there is a LOT of code here to play with

	A binned binary tree is a binary tree with lists as data points
	- often use the bins to store duplicate values
	- could use the bins to store values within some range of the head element of the bin
		+ but this wouldn't yeild a very useful search speed increase

	To define this, we'll want a Binary Tree and a List predefined.

	We'll create our own BinaryTree and our own LinkedList
	- while Scala's native List datatype would be sufficient, that introduces methods
		and a bunch of extra terminology that can be distracting
*/


/*
	We should test our work.
	In development, I like to have lots of different tests available.
	Here the only thing that I call is testAll. To change what is tested, I could just edit testAll
*/

def testAll():Int = {
	println("Running all tests")
	println("if an error is thrown then something is wrong")
	testLL()
	testBT()
	testBTLL()
	println("No errors were thrown!!")
	0
}

/*
	I'll start with the "easier" datatype: Linked Lists
*/
// LinkedList of Int
sealed abstract class LLInt
case object LLIntEnd extends LLInt
case class LLIntNode(h:Int, t:LLInt) extends LLInt

// that's fine but not super extensible, lets create a LL of any type
sealed abstract class LL[+A]
case object LLEnd extends LL
case class LLNode[A](d:A, t:LL[A]) extends LL[A]

// ASIDE: now if I want an LL of ints, I can make that pretty easily
val aside_my_LL_Int:LL[Int] = LLNode(1,LLEnd)

/*
	Now that we have the data type we might want a few HOF for it.
	We'll curry the inputs
	We'll keep the types abstract for extensibility
	I'm also writing tests as I go, you'll see them later on
*/
def foldLeftLL[A,B](l:LL[A])(acc:B)(cb:(B,A)=>B):B = l match {
	case LLEnd => acc
	case LLNode(h,t) => {
		val accp = cb(acc,h)
		val accpp = foldLeftLL(t)(accp)(cb)
		accpp
		//foldLeftLL(t)(cb(acc,h))(cb)
	}
}

def foldRightLL[A,B](l:LL[A])(acc:B)(cb:(A,B)=>B):B = l match {
	case LLEnd => acc
	case LLNode(h,t) => {
		// must look at tail first!
		val accp = foldRightLL(t)(acc)(cb)
		val accpp = cb(h,accp)
		accpp
		// cb(h,foldRightLL(t)(acc)(cb))
	}
}

def mapLL[A,B](l:LL[A])(cb:(A)=>B):LL[B] = {
	// using fold, need foldRight
	foldRightLL(l)(LLEnd:LL[B]){
		(h,acc) => LLNode(cb(h), acc)
	}
}
// direct mapLL
// l match {
// 	case LLEnd => LLEnd // can't return `l`, becuase of type issues
// 	case LLNode(h,t) => {
// 		val hp = cb(h)
// 		val tp = mapLL(t)(cb)
// 		val lp = LLNode(hp,tp)
// 		lp
// 		// LLNode(cb(h), mapLL(t)(cb))
// 	}
// }

def existsLL[A](l:LL[A])(cb:(A)=>Boolean):Boolean = {
	// using an HOF
	foldLeftLL(l)(false){(acc,h) => acc || cb(h) } // { _ || cb(_) }
}
// //direct extistsLL
// l match {
// 	case LLEnd => false
// 	case LLNode(h,t) => cb(h) || existsLL(t)(cb) // short curcuits!
// 	// VV these are fine... but ^^ is more cleaver
// 	// case LLNode(h,_) if cb(h) => true
// 	// case LLNode(_,t) => existsLL(t)(cb)
// }

//UNTESTED
def findLL[A](l:LL[A])(cb:(A)=>Boolean):Option[A] = {
	foldLeftLL(l)(None:Option[A]){
		case (acc@Some(_),_) => acc
		case (None, h) => if (cb(h)) Some(h) else None
	}
}

/*
	in addition to the HOF definitions, I might want some functions that use them
*/
def sumLL(l:LL[Int]):Int = {
	// TODO: improve definition to work on a more abstract type
	// returns 0 if l is empty
	foldLeftLL(l)(0){ (acc,h) => acc + h } // { _ + _ }
}

def incAllBy1LL(l:LL[Int]):LL[Int] = {
	// TODO: improve definition to work on a more abstract type
	mapLL(l){ (h) => h + 1 } // { _ + 1 }
}

def hasOddLL(l:LL[Int]):Boolean = {
	// TODO: improve definition to work on a more abstract type
	existsLL(l){ (h) => h % 2 == 1 } // { _ % 2 == 1 }
}

/* 
	Now for some TESTS on LL
*/

def testLL():Int = {
	testFoldLeftLL()
	testMapLL()
	testExistsLL()
	0	
}

def testFoldLeftLL():Int = {
	val l0:LL[Int] = LLEnd
	val l1:LL[Int] = LLNode(5,l0)
	val l2:LL[Int] = LLNode(15,l1)
	val l3:LL[Int] = LLNode(25,l2)
	assert( sumLL(l0) == 0 )
	assert( sumLL(l3) == 45 )
	0
}

def testMapLL():Int = {
	val l0:LL[Int] = LLEnd
	val l1:LL[Int] = LLNode(5,l0)
	val l2:LL[Int] = LLNode(15,l1)
	val l3:LL[Int] = LLNode(25,l2)
	assert( incAllBy1LL(l0) == l0 )
	assert( incAllBy1LL(l3) == LLNode(26, LLNode(16, LLNode(6, LLEnd))) )
	0
}

def testExistsLL():Int = {
	val l0:LL[Int] = LLEnd
	val l1:LL[Int] = LLNode(5,l0)
	val l2:LL[Int] = LLNode(15,l1)
	val l3:LL[Int] = LLNode(25,l2)
	assert( hasOddLL(l3) )
	assert( !hasOddLL(incAllBy1LL(l3)) )
	0
}

/*
	And now the more complicated structure, TREES, Ill name it BT for binary tree
	Here I'll get lazy and stop testing, because I'm confident and need to power through
	But you could write your own tests
	After you have your tests writen... You might want to also rewrite my code to 
	something more legible, I wrote the fast versions...
*/

sealed abstract class BT[+A]
case object BTEnd extends BT
case class BTNode[A](l:BT[A],d:A,r:BT[A]) extends BT[A]

def foldLeftBT[A,B](t:BT[A])(acc:B)(cb:(B,A)=>B):B = t match {
	case BTEnd => acc
	case BTNode(l,d,r) => foldLeftBT(r)(cb(foldLeftBT(l)(acc)(cb),d))(cb)
}

def foldRightBT[A,B](t:BT[A])(acc:B)(cb:(A,B)=>B):B = t match {
	case BTEnd => acc
	case BTNode(l,d,r) => foldRightBT(l)(cb(d,foldRightBT(r)(acc)(cb)))(cb)
}

def mapBT[A,B](t:BT[A])(cb:(A)=>B):BT[B] = t match { // fold can't really be used for tree
	case BTEnd => BTEnd:BT[B]
	case BTNode(l,d,r) => BTNode(mapBT(l)(cb),cb(d),mapBT(r)(cb))
}

def existsBT[A](t:BT[A])(cb:(A)=>Boolean):Boolean = {
	foldLeftBT(t)(false){ (acc,h) => acc || cb(h) }
}

def findBT[A](t:BT[A])(cb:(A)=>Boolean):Option[A] = {
	foldLeftBT(t)(None:Option[A]){
		case (acc@Some(_),_) => acc
		case (None, h) => if (cb(h)) Some(h) else None
	}
}

def testBT():Int = {
	// write and test sumBT using foldLeftBT and foldRightBT
	// write and test incAllByN using foldLeftBT
	// write and test hasOddBT using existsBT
	0
}


/*
	and finally the Binned binary tree
		a BT of LL
	Ill name it BinT
*/
type BinT[A] = BT[LL[A]]

def foldLeftBinT[A,B](t:BinT[A])(acc:B)(cb:(B,A)=>B):B = {
	foldLeftBT(t)(acc){
		(accp,h) => foldLeftLL(h)(accp)(cb)
	}
}

def directFoldLeftBinT[A,B](t:BinT[A])(acc:B)(cb:(B,A)=>B):B = {
	def helper(l:LL[A])(acc:B):B = l match {
		case LLEnd => acc
		case LLNode(h,t) => {
			val accp = cb(acc,h)
			val accpp = helper(t)(accp)
			accpp
		}
	}
	t match {
		case BTEnd => acc
		case BTNode(l,d,r) => {
			val accp = directFoldLeftBinT(l)(acc)(cb)
			val accpp = helper(d)(accp)
			val accppp = directFoldLeftBinT(r)(accpp)(cb)
			accppp
		}
	}
}

def foldRightBinT[A,B](t:BinT[A])(acc:B)(cb:(A,B)=>B):B = {
	acc // obviously wrong... complete any way you want...
}

def mapBinT[A,B](t:BinT[A])(cb:(A)=>B):BinT[B] = {
	BTEnd:BinT[B] // obviously wrong... complete any way you want...
}

def existsBinT[A](t:BinT[A])(cb:(A)=>Boolean):Boolean = {
	false // obviously wrong... complete any way you want...
}

def findBinT[A](t:BinT[A])(cb:(A)=>Boolean):Option[A] = {
	None // obviously wrong... complete any way you want...
}

// sum all values in the binned tree
def sumBinT(t:BinT[Int]):Int = foldLeftBinT(t)(0){ _ + _}
// product of all values in binned tree
def productBinT(t:BinT[Int]):Int = foldLeftBinT(t)(1){ _ * _}

/*
	imagine writing those directly... 0.o... WAT?
*/
def directSumBinT(t:BinT[Int]):Int = 0
def directProductBinT(t:BinT[Int]):Int = 1



def validBinT(t:BinT[Int]):Boolean = {
	/*
		a binned tree is valid iff
			all bins are nonempty
			all bins contain only one value `v` 0-or-many times
			the `v` found when folding left to right over the tree must be strictly increasing

		some subproblems
			isEmptyLL(_:LL[A]):Boolean
			allSameLL(_:LL[A]):Boolean
			getHead(_:LL[A]):A

		a smarter subproblem
			findAllSameLL(_:LL[A]):Option[A]

		in functional programming, it is often nice to decompose the problem into whatever
		subproblems that you can, then you have lots of different funcitons that work correctly
		this, when done correctly, will quickly grow your code base

		known bug: doesn't work if the smallest valin the tree is Int.MinValue
	*/
	val ov = foldLeftBT(t)(Some(Int.MinValue):Option[Int]){
		case (None, _) => None
		case (Some(min),l) => {
			findAllSameLL(l) match {
				case None => None
				case Some(d) if (d <= min) => None
				case someMinPrime => someMinPrime
			}
		}
	}
	ov != None

	/*
		map[A](_:Collection[A])(_:(A)=>B):Collection[B]
		flatMap[A](_:Collection[A])(_:(A)=>Collection[B]):Collection[B]
	*/
	// foldLeftBT(t)(Some(Int.MinValue):Option[Int]){
	// 	(acc,l) => acc flatMap {
	// 		(min) => findAllSameLL(l) map { 
	// 			case (d) if (d < min) => d
	// 		}
	// 	}
	// } != None
}

def findAllSameLL[A](l:LL[A]):Option[A] = l match {
	case LLEnd => None
	case LLNode(h,t) => if (existsLL(t){ _ != h }) None else Some(h)		
}


def testBTLL():Int = {
	0
}

testAll





