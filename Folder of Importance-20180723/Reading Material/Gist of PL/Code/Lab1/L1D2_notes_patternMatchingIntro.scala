/* Here we will write a simple function that does not demonstrate
 the true usefulness of pattern matching but I think is a useful 
 springboard to a broader conversation */

def f(x:Double):String = x match {
	case 0.0 => "The number was 0"
	case 5.0 => "The number was 5"
	case _ => "The number was not 0 or 5"
}
/* Here we essentially wrote a switch statement. 
If the input to f is 0.0 then the function returns
"The number was 0". If the input is 5.0 then the 
functions returns "The number was 5". And otherwise
the function returns "The number was not 0 or 5"*/
/* and you are likely asking yourself... so what?
can't I just write this with conditional expressions?
And you're correct. You can. Give or take some additional
scopes it would look a bit like :*/
def fWithoutPatternMatching(x:Double):String = {
	if (x == 0.0) {
		"The number was 0"
	} else {
		if (x == 5.0) {
			"The nubmer was 5"
		} else {
			"The number was not 0 or 5"
		}
	}
}
/* 
here pattern matching has some benefits.
   - it is arguably cleaner code
   - it has more Parallel-ability
*/

/* Now this is not why pattern matching is so useful 
Consider the following function that takes as input
a three valued tuple each of type Double*/
def g(x:(Double,Double,Double)):String = x match {
	case (_,_,0.0) => "Finished"
	case (5.0,b,1.3) => g(b,b,1.7)
	case (a,_,1.7) => g(5.1,a,a)
	case (a,b,c) => g(a*b,b+c,0.0)
	case _ => "The function Failed"
}
/* Take a minute to analyze this code and gues at what
 it will do. After you have thought about it for a bit,
 try it out in a Scala worksheet and see if you were
 correct. Below, I have provided a few test cases for 
 you to consider.*/

val v0 = g((1.0,2.0,3.0))
val v1 = g((5.0,20,1.3))
