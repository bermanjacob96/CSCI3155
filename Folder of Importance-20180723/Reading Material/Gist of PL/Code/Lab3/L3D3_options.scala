// L3D3_notes.scala
/*
	Using a Linked List
	iff all the values are even then tell
	me the summation of the list
	else tell me that there was an odd number
	and tell me that you couldn't find the value
	by either returning None or throwing an error
 */

// Linked List definition
sealed abstract class LL
case object Empty extends LL
case class Node(d:Int,lp:LL) extends LL

// with tuple of Boolean and LL
// returns Int or blows up
def f(l:LL):Int = {
	def inner(in:(Boolean,LL)):Int = in match {
		case (false,_) => {
			// state where the problem happend
			println("there was an odd value in l at : " + l)
			// blow up
			??? 
		}
		// now the first value must be true, I don't need to match on it but I will anyway
		case (true,Empty) => 0
			// note that this case statement uses a "guard" i.e. the if statement BEFORE the arrow
			// it only does that case body IF it has the correct pattern and passes the gaurd
		case (true,Node(d,lp)) if ( d % 2 == 0 ) => {
			d + inner((true,lp))
		}
		case (true,l) => inner((false,l))
	}
	inner((true,l)) // assumes innocent
}


/* 
	Perhaps less elegant to read but significantly more
	useful when integrated into large systems ( since I don't
	have to deal with catching errors )
 */
def g(l:LL):Option[Int] = {
	def inner(in:(Boolean,LL)):Option[Int] = in match {
		case (false,_) => {
			// state where the problem happend
			println("there was an odd value in l at : " + l);
			// return something useful 
			// rather than blowing up with ???
			None
		}
		// now the first value must be true, I don't need to match on it but I will anyway
		case (true,Empty) => Some(0)
		case (true,Node(d,lp)) if ( d % 2 == 0 ) =>{
			// Note how the flow of control has changed
			inner((true,lp)) match {
				case None => None
				case Some(acc) => Some(d + acc)
			}
		}
		case (true,l) => inner((false,l))
	}
	inner((true,l)) // still assumes innocent
}



// For the heck of it lets look at 1 more function to complete the work
// observe that this function no longer tells me where the odd number was found
def h(l:LL):Option[Int] = {
	def inner(in:Option[LL]):Option[Int] = in match {
		case None => {
			// state where the problem happend
			println("there was an odd value in l at : " + l);
			// return something useful 
			// rather than blowing up with ???
			None
		}
		// now the first value must be true, I don't need to match on it but I will anyway
		case Some(Empty) => Some(0)
		case Some(Node(d,lp)) if ( d % 2 == 0 ) =>{
			// Note how the flow of control has changed
			inner(Some(lp)) match {
				case None => None
				case Some(acc) => Some(d + acc)
			}
		}
		case Some(l) => inner(None)
	}
	inner(Some(l))// assume innocent
}

/* ----------- TESTING -------------- */
def testf() = {
	println("\nTesting f")
	val evenList : LL = Node(2,Node(18,Empty))
	val withOddList : LL = Node(2,Node(5,Empty))
	println(f(evenList))
	// throws error
	// println(f(withOddList))
}
def testg() = {
	println("\nTesting g")
	val evenList : LL = Node(2,Node(18,Empty))
	val withOddList : LL = Node(2,Node(5,Empty))
	println(g(evenList))
	// Does not throw error
	println(g(withOddList))
}
def testh() = {
	println("\nTesting h")
	val evenList : LL = Node(2,Node(18,Empty))
	val withOddList : LL = Node(2,Node(5,Empty))
	println(h(evenList))
	println(h(withOddList))
}
def tests() = {
	println("\n\n\n\n----------- begin TESTING -------------\n")
	testf()
	testg()
	testh()
	println("\n----------- end TESTING -------------\n\n\n\n")
}
tests()