// NOTE : I only performed minimal testing on this code!

/*
 Exercise

   Write a method for our LinkedList data structure that determines if the
   data structure is in decending order
     - using 2 cases and a conditional expression
     - using only 1 case and a gaurd
     - using boolean logic

   Write some minimal test cases for the isDesending method

 Lab goal

   complete part 6 on BST
   complete question 2 on pattern matching
*/
/*
	When learning a new Programming Language I often find it useful
	to see how a linked list is coded in that language.

	Here is a small library for a linked list over integers in scala

	Note: there are many other ways that one could implement this structure.
	You are encouraged to poke around at this code to learn more and find
	other ways to exploit the data structure. You might start by porting this
	code to a worksheet in IntelliJ
 */
// The Grammar
/*
	S ::= End | n -> S
	n is a metavariable that represents numbers
 */

// Define the data structure
sealed abstract class LinkedList
case object Empty extends LinkedList
case class Node(n:Int,tail:LinkedList) extends LinkedList

// a method to print a LikedList
// 	For a LinkedList of length m st m > 0
// 		n_0 -> n_1 -> ... -> n_m-2 -> n_m-1 -> Empty
// 	For a LinkedList of length 0
// 		Empty
def print(ll:LinkedList):Unit = {
  def mySpecialLoop(ll:LinkedList):String = ll match {
    case Empty => "Empty"
    case Node( current_n , current_tail ) => current_n.toString + " -> "
  }
  println(mySpecialLoop(ll))
}

// a method to prepend a data point to the structure
def prepend(ll:LinkedList, n:Int ):LinkedList = Node( n , ll )

// a method to append a data point to the structure
def append(ll:LinkedList, n:Int ):LinkedList = ll match {
  case Empty => Node(n,Empty)
  case Node(current_n, current_tail) => {
    // val current_tailp = append(current_tail,n)
    // Node(current_n, current_tailp)
    Node(current_n, append(current_tail,n))
  }
}

// a method to insert a new data point to the structure in order (ascending)
/*
	Here we are not allocating new memory for the structure and thus we are not
	actually changing the input 'll' but rather we are constructing a new
	LinkedList that will look quite a bit like ll but with n inside of it
	lets call that llp.

	Note that the second case has a condition before the '=>'. This is called a guard
	optional exercise: What happens if you take the second case and its case body and
		moved it to make it the last case and case body?
	optional exercise: Write this code without a guard using only 2 cases.
 */
def insert(ll:LinkedList, n:Int):LinkedList = ll match {
  case Empty => Node( n , Empty )
  case Node( current_n , current_tail ) if ( current_n < n ) => {
    // val current_tailp = insert(current_tail,n)
    // Node( current_n , current_tailp )
    Node( current_n , insert( current_tail , n ) )
  }
  case Node( current_n , current_tail ) => Node( n , ll )
}

// a method to determine if the data structures is in ascending order
// 	here an Empty structure is considered to be ordered
/*
	Here I changed the naming scheme because I felt like it.
	When coding you should use names that make sense to you.
	I use 'n' to represent the integer
	and 'llp' to represent the rest of the linked list
 */
def isAscending(ll:LinkedList):Boolean = ll match {
  case Empty => true
  case Node(n,llp) => {
    def myLoop(ll:LinkedList,n:Int):Boolean = ll match {
      case Empty => true
      case Node(np,llp) => ( n <= np ) && myLoop(llp,np)
      /*
        PLEASE : Don't write your code like this commented out part

        Or rather... if you are not comfortable with boolean
        expressions and logic then you SHOULD start by writing code
        like that seen below. But then you should refine the code
        until you end up with something cleaner (like the line above)
       */
      // {
      // 	if (( n <= d ) == true ) {
      // 		if (myLoop(llp,d) == true) {
      // 			true
      // 		} else {
      // 			false
      // 		}
      // 	} else {
      // 		false
      // 	}
      // }
    }
    myLoop(llp,n)
  }
}

// a function to test our work
/*
	Ideally this is not written in the same script but rather it is
	written as part of a separate testing script

	optional exercise : extend this testing script
	optional exercise : add some visual print testing into the script
	optional exercise : write a new testing function
 */
def myTest():Unit = {
  val l0 = Empty
  val l1 = Node(20,Empty)
  val l2 = Node(10,Node(20,Empty))
  val l3 = Node(15,Node(10,Node(20,Empty)))
  val l4 = Node(10,Node(15,Node(20,Empty)))
  val l5 = Node(10,Node(20,Node(15,Empty)))

  assert(l1 == insert(l0,20))

  assert(l2 == insert(l1,10))

  assert(l3 == prepend(l2,15))

  assert(l4 == insert(l2,15))

  assert(l5 == append(l2,15))

  assert(false == isAscending(l3))

  assert(true == isAscending(l4))

  assert(false == isAscending(l5))

}

myTest()





