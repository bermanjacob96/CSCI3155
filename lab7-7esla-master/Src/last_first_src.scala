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

// BTLLfoldLeft

// BTLLmap

// sumBinnedTree

// isBinnedSearchTree


/*

	Q2.

 */
// getByPolicy

// getEvens

// getNonNegs

// getConsonants


/*

	Q3.

 */
// findByPolicyIff

// getFirstFibSeqSTLenGeN
