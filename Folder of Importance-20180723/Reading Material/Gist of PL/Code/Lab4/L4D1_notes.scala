// L4D1_notes.scala

/* example list */
val myList:List[Int] = 1 :: 2 :: 3 :: Nil

/* basic print */
println(myList)

/* basic pattern match */
myList match {
	case h :: t => println(h); println(t)
	case _ => ??? // unreachable code for this example
}

/* a function that performs summing directly over a List */
def sum(l:List[Int]):Int = l match {
	case Nil => 0
	case h :: t => h + sum(t)
}
println(sum(myList))

/*
def fold[A,B](l:List[A])(z:B)(cb:(B,A)=>B):B = {
	def loop(lp:List[A]):B = lp match{
		case Nil => z
		case h :: t => cb(loop(t),h)
	}
	loop(l)
}

val out = myList.fold(0)({
	(acc,h) => {
		h + acc
	}
})
println(out)