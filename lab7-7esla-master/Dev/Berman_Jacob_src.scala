// L7submition.scala

/*

	Q1.

 */
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

def BTLLmap[A,B](t:BinT[A])(cb:A=>B):BinT[B] = t match {
	case Empty => Empty:BinT[B]
	case BTNode(l, d, r) => BTNode(BTLLmap(l)(cb), d.map(cb) ,BTLLmap(r)(cb))
}

def sumBinnedTree(t:BinT[Int]):Int = {
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


/*

	Q2.

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

	Q3.

 */
def findByPolicyIff[A,B](l:List[A])(policy:(A) => Option[B]):Option[List[B]] = None

// getFirstFibSeqSTLenGeN
def getFirstFibSeqSTLenGeN(l:List[Int])(n:Int):Option[List[Int]] = {
	def loop(l:List[Int])(optionFib1:Option[Int])(optionFib2:Option[Int])(lengthObserved:Int)(sc:Option[List[Int]] => Option[List[Int]]): Option[List[Int]] = l match {
		case Nil => if (lengthObserved >= n) { sc(Some(Nil)) } else { None }
		case h :: t => optionFib1 match {
			case None => {
				if (h == 1) {
					// a great start we've found <STUFF>::<NOT 1>::1::<MORE STUFF>
					loop(t)(Some(h))(None)(1){ _ flatMap { tp => sc(Some(h :: tp)) }

						// option_tp => option_tp map { 
						// 	tp => h :: tp 
						// }

						// option_tp match {
						// 	case None => None
						// 	case Some(tp) => Some(h :: tp)
						// }
					}
				}
				else {
					// reset, we failed so far
					loop(t)(None)(None)(0)( optionFibList => optionFibList )
				}
			}
			case Some(fib1) => {
				optionFib2 match {
					case None => {
						// require( fib1 == 1 )
						// require( lengthObserved == 1 )
						if ( h == 1 ) {
							// we're off to a good start, we've observer <STUFF>::<NOT 1>::1::1::<MORE STUFF>
							loop(t)(optionFib1)(Some(h))(2){ _ flatMap { tp => sc(Some(h :: tp))} }
						}
						else {
							// reset, we failed so far
							loop(t)(None)(None)(0)( optionFibList => optionFibList )
						}
					}
					case Some(fib2) => {
						if (fib1 == 1 && fib2 == 1 && h ==1) {
							// require( lengthObserved == 2 )
							// edge case... power through
							loop(t)(optionFib1)(optionFib2)(lengthObserved)(sc)
						}
						else {
							val nextFib = fib1 + fib2
							if (h == nextFib) {
								// we can keep looking for a longer fib list
								loop(t)(optionFib2)(Some(h))(lengthObserved + 1){ _ flatMap { tp => sc(Some(h :: tp)) } }
							}
							else if (lengthObserved >= n) {
								// were done
								sc(Some(Nil))
							}
							else {
								// reset, we failed so far
								loop(t)(None)(None)(0)( optionFibList => optionFibList )
							}
						}
					}
				}
			} 
		}
	}
	loop(l)(None)(None)(0)( optionFibList => optionFibList )
}


def testGetFib() = {
	val l0 = List(100,12, 1,1,2,3,5,   20,16)
	val v0 = getFirstFibSeqSTLenGeN(l0)(3)
	assert(v0 == Some(List(1,1,2,3,5)) )
	val v1 = getFirstFibSeqSTLenGeN(l0)(7)
	assert(v1 == None)

	val l1 = List( 1,1,100,2, 12,3,20,5,60, 1,1,2, 50 )
	assert(getFirstFibSeqSTLenGeN(l1)(2) == Some(List(1,1)) )
	assert(getFirstFibSeqSTLenGeN(l1)(3) == Some(List(1,1,2)) )
	assert(getFirstFibSeqSTLenGeN(l1)(4) == None )
