// What does myFunction return on input: 
// C(B(Fish,A(Cat)),Dog,Dog)
// C(Dog,Fish,A(Cat))
// Cat
sealed abstract class MyObj
case object Dog extends MyObj
case object Cat extends MyObj
case object Fish extends MyObj
case class A(o1:MyObj) extends MyObj
case class B(o1:MyObj,o2:MyObj) extends MyObj
case class C(o1:MyObj,o2:MyObj,o3:MyObj) extends MyObj

def myFunction(o:MyObj):MyObj = {
	o match {
		case A(_) => Dog
		case B(o1,o2) => myFunction(C(A(o1),B(o2,o2),o2))
		case C(o1,Fish,_) => B(Fish,o1)
		case C(Cat,_,o3) => myFunction(o3)
		case C(o1,o2,o3) => myFunction(A(o1))
		case _ => o
	}
}


/*
now for some linked lists...
*/

// Use this definition of Linked List
sealed abstract class LL
case object Nil extends LL
case class Node(_:String, _:LL) extends LL

// define isDescending over LL

// write some assersions....



