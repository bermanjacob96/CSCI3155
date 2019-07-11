/*
Lets talk about pattern matching.
*/

// What does this function DO?
def foo(bar:(Int, Int, Int)):Int = bar match {
	case (_, _, 0) => 42
	case (1,2,0) => 0
	case (a,1,_) if (a % 2 == 1) => foo((a+1, a, 0))
	case (b,1,_) => foo((b, b, 1))
	case (1,2,c) => foo((1, 2, 0))
}

assertResult(42)(foo((20,22,0)))
// Let's write more assertions



/*
This gets a LOT more interesting with a data structure. Let's
write stuff over a linked list today
*/

sealed abstract class LinkedList
case object Empty extends LinkedList
case class Node(head:Int,tail:LinkedList) extends LinkedList

def print(l:LinkedList):Unit = ???

def isAscending(l:LinkedList):Boolean = ???

def isDeccending(l:LinkedList):Boolean = ???

def isOrdered(l:LinkedList):Boolean = ???

// what are a few others that we could write?

// Can we apply this to other concepts?