// L7Q3.scala

/*
	Continuations

	Implement the following code according to the specification provided in your
	lab 7 handout.
*/

def findByPolicyIff[A,B](l:List[A])(policy:(A) => Option[B]):Option[List[B]] = l match {
	case Nil => None
	case h::t => 
}
// TODO

def getFirstFibSeqSTLenGeN(l:List[Int])(n:Int):Option[List[Int]] = {
	require(n > 2)
	def loop(l: List[Int])(n:Int)(sc:Int => Int)(fc: () => Boolean):Option[List[Int]] = {
		case Nil => fc()
		case h :: t =>  { sc(h) || loop(t)(acc => sc(acc+h))(() => myLoop(t)(acc => acc)(fc)) }
	}
	
	loop(l)(r => r)(()=>false)
}
// testing
// TODO