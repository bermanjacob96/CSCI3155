









def mulListDirect(l: List[Int]): Int = l match {
	case Nil => 1
	case h :: t => h * mulListDirect(t)
}

// mulListDirect([5, 2]) -->* 
// 5 * mulListDirect([2]) -->* 
// 5 * 2 * mulListDirect([]) --> *
// 5 * 2 * 1 -->*
// 10

[0, 5, 6 ,2 , 18 ,25 ]













def mulListDirectShortCircuit(l: List[Int]): Int = l match {
	case Nil => 1
	case 0 :: _ => 0
	case h :: t => h * mulListDirectShortCircuit(t)
}


// mulListDirectShortCircuit([0,5,6,12]) -->*
// 0

// mulListDirectShortCircuit([2,3,0,5,8,12]) --> *
// 2 * mulListDirectShortCircuit([3,0,5,8,12]) --> *
// 2 * 3 * mulListDirectShortCircuit([0,5,8,12]) --> *
// 2 * 3 * 0 --> *
// 0











def mulListResultAcc(l: List[Int]): Int = {
	def mul(acc: Int, l: List[Int]): Int = l match {
		case Nil => acc
		case h :: t => mul(acc * h, t)
	}
	mul(1, l)
}

// mulListResultAcc([2,3]) -->*
// mul(1, [2,3]) -->*
// mul(1*2, [3]) -->*
// mul(1*2*3, []) -->*
// 1*2*3 -->*
// 6












def mulListResultAccShortCircuit(l: List[Int]): Int = {
	def mul(acc: Int, l: List[Int]): Int = l match {
		case Nil => acc
		case 0 :: _ => 0
		case h :: t => mul(acc * h, t)
	}
	mul(1, l)
}

// mulListResultAccShortCircuit([5,0,6]) -->*
// mul(1, [5,0,6]) -->*
// mul(1*5, [0,6]) -->*
// 0

// mulListResultAccShortCircuit([5,0,6]) -->*
// mul(1, [5,0,6]) -->*
// mul(1 * 5, [0,6]) -->*
// mul(5, [0,6]) -->*
// 0













def mulListDelayed(l: List[Int]): Int = {
	def mul(sc: Int => Int, l: List[Int]): Int = l match {
		case Nil => sc(1)
		case h :: t => mul(acc => sc(h * acc), t)
	}
	mul(r => r, l)
}










def mulListDelayedShortCurcuit(l: List[Int]): Int = {
	def mul(sc: Int => Int, l: List[Int]): Int = l match {
		case Nil => sc(1)
		case 0 :: t => 0
		case h :: t => mul(acc => sc(h * acc), t)
	}
	mul(r => r, l)
}













