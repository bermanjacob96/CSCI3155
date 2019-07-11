// L4D2_notes.scala

val l:List[Int] = 1 :: 2 :: 3 :: Nil

// comment and uncomment as you see fit.
// summateTest()
// sideEffectTest()
// failedDoubling()
// correctDoubling()
// testMap()
// testForEach()
// testExists()
testMapHOF()
// testAll()

def summateTest() = {
	println("\n\n\n--------------------------------\nBegin Testing summateTest\n")
	println("\nfold")
	println(l.fold(0){(acc,h) => h+acc})
	println("\nfoldLeft")
	println(l.foldLeft(0){(acc,h) => h+acc})
	println("\nfoldRight")
	println(l.foldRight(0){(h,acc) => h+acc})
	println("\n\n\nEnd Testing summateTest\n--------------------------------\n")

}
def sideEffectTest() = {
	println("\n\n\n--------------------------------\nBegin Testing sideEffectTest\n")
	println("\nfold")
	l.fold(()){(acc,h) => println(h)}
	println("\nfoldLeft")
	l.foldLeft(()){(acc,h) => println(h)}
	println("\nfoldRight")
	l.foldRight(()){(h,acc) => println(h)}
	println("\nforEach")
	l.foreach{ (h) => println(h) }
	println("\n\n\nEnd Testing sideEffectTest\n--------------------------------\n")
}
def failedDoubling() = {
	println("\n\n\n--------------------------------\nBegin Testing failedDoubling\n")
	// println("\nfold")
	// fold does weird stuff so ignor it
	// println(l.fold(Nil:List[Int]){(acc,h) => h::acc })
	println("\nfoldLeft")
	val lpL = l.foldLeft(Nil:List[Int]){(acc,h) => (h*2)::acc}
	println(lpL)
	println("\nfoldRight")
	val lpR = l.foldRight(Nil:List[Int]){(h,acc) => (h*2)::acc}
	println(lpR)
	println("\nmap")
	val lpM = l.map{(h) => h* 2 }
	println(lpM)
	println("\n\n\nEnd Testing failedDoubling\n--------------------------------\n")
}
def correctDoubling() = {
	println("\n\n\n--------------------------------\nBegin Testing correctDoubling\n")
	println("\nfoldLeft")
	val lpL = l.foldLeft(Nil:List[Int]){(acc,h) => acc:::List((h*2))}
	println(lpL)
	println("\nfoldRight")
	val lpR = l.foldRight(Nil:List[Int]){(h,acc) => (h*2)::acc}
	println(lpR)
	println("\nmap")
	val lpM = l.map{(h) => h* 2 }
	println(lpM)
	println("\n\n\nEnd Testing correctDoubling\n--------------------------------\n")
}
def testMap() = {
	println("\n\n\n--------------------------------\nBegin Testing testMap\n")
	println("\nmap")
	val lpM = l.map{(h) => h* 2 }
	println(lpM)
	println("\n\n\nEnd Testing testMap\n--------------------------------\n")
}
def testForEach() = {
	println("\n\n\n--------------------------------\nBegin Testing testForEach\n")
	println("\nforEach")
	l.foreach{ (h) => println(h) }
	println("\n\n\nEnd Testing testForEach\n--------------------------------\n")
}

def testExists() = {
	println("\n\n\n--------------------------------\nBegin Testing testExists\n")
	println("\ncontains odd values?")
	val b0 = l.exists{ (h) => (h%2) == 1 }
	println(b0)
	println("\nContains negative values?")
	val b1 = l.exists{ (h) => h < 0 }
	println(b1)
	println("\n\n\nEnd Testing testExists\n--------------------------------\n")
}
def testMapHOF() =  {
	println("\n\n\n--------------------------------\nBegin Testing testMapHOF\n")
	// valid... and I think its easy to read
	val m:Map[String,Int] = Map.empty + ( "x" -> 2 ) + ("y" -> 5)
	// also valid, but more lengthy
	def extend[A,B](m:Map[A,B],key:A, value:B) = { m + (key -> value) }
	val tinyMap:Map[String,Int] = Map.empty
	val bigerMap:Map[String,Int] = extend(tinyMap,"x",2)
	val evenBiggerMap:Map[String,Int] = extend(bigerMap,"y",5)
	val m2:Map[String,Int] = extend(extend(Map.empty,"x",2),"y",5)
	//recall that maps are non-mutable objects
	println("\nThe map and its submaps")
	println(tinyMap)
	println(bigerMap)
	println(evenBiggerMap)
	println(m)
	println(m2)
	println("\nusing the map method and HOF of the Map data structure")
	val mdoubled0 = m.map { 
		h => h match {
			case ((x,n)) => (x,n*2)
		} 
	}
	println("\tdoubled the values")
	println("\t\t"+mdoubled0)
	// OR... if I am cool
	val mdoubled1 = m.map{ case ((x,n)) => (x,n*2) }
	println("\t\t"+mdoubled1)

	println("\tChanging the Key type")
	var i = 0
	val mp0 = m.map{
		h => h match {
			case (x,n) => {
				i = i + 1
				(i,n)
			}
		}
	}
	println("\t\t"+mp0)
	// Or if you're cool
	i = 0
	val mp1 = m.map{ case ((x,n)) => i = i + 1 ; (i,n) }
	println("\t\t"+mp1)


	// note that m itslef has not changed
	println("\tNote: doesn't change the original map")
	println("\t\t"+m)

	println("\tdoubled the values using mapValues")
	val mdoubled2 = m.mapValues{ n => n*2 }
	println("\t\t"+mdoubled2)

	// Get is not HOF over Maps but it is worth knowing about
	println("\n\nUsing the get method vs direct access")
	val nx = m.get("x")
	println(nx)
	println(m.get("y"))
	println(m.get("z")) // safe
	println(m("x"))
	println(m("y"))
	// println(m("z")) // not so safe... throws error

	println("\n\n\nEnd Testing testMapHOF\n--------------------------------\n")
}
def testAll() {
	summateTest()
	sideEffectTest()
	failedDoubling()
	correctDoubling()
	testMap()
	testForEach()
	testExists()
	testMapHOF()
}