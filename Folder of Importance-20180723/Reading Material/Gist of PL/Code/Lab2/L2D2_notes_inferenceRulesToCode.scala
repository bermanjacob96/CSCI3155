// S ::= S ^ S | ~S | b
sealed abstract class MYS
case class B(b:Boolean) extends MYS
case class Not(s1:MYS) extends MYS
case class Xor(s1:MYS, s2:MYS) extends MYS

// Typset rules
// RULE1
	// C : S1 ^ S2 \Downarrow b
	// J0 : S1 \Downarrow b1
	// J1 : S2 \Downarrow b2
	// J2 : b = b1 ^ b2

// RULE2
	// C : ~S1 \Downarrow b
	// J0 : S1 \Downarrow b1
	// J1 : b = ~ b1¬

// RULE 3
	// C :  b \Downarrow b
	// S \Downarrow b

def eval(s:MYS):Boolean = s match {
	// RULE3
	// C :  b \Downarrow b
	case B(b) => { // b left of \Downarrow
		b // b right of \Downarrow
	}
	// RULE2
	// C : ~S1 \Downarrow b
	// J0 : S1 \Downarrow b1
	// J1 : b = ~ b1
	case Not(s1) => { // ~S1 left of \Downarrow in C
		val b1 = eval(s1) // J0
		val b = ! b1 // J1 Scala bitwise not on a boolean can be done with logical not
		b // b right of \Downarrow in C
	}
	// RULE1
	// C : S1 ^ S2 \Downarrow b
	// J0 : S1 \Downarrow b1
	// J1 : S2 \Downarrow b2
	// J2 : b = b1 ^ b2
	case Xor(s1,s2) => { // S1 ^ S2 left of \Downarrow in C
		val b1 = eval(s1) // J0
		val b2 = eval(s2) // J1
		val b = b1 ^ b2 // J2
		b // b right of \Downarrow in C
	}

}

// a few tests
assert(eval(B(true)) == true)
assert(eval(Not(B(true))) == false)
assert(eval(Xor(B(true), B(true))) == false)
