sealed abstract class S
case object a extends S
case class bc(s:S) extends S
case class cc(t:T, s1:S, s2:S) extends S

sealed abstract class T
case object Nil extends T
case class a(t:T) extends T


def angryFace(s:S): Int = s match {
	case `a` => 0 // on the exam you can write this without the backtic
	case bc(s1) => angryFace(s1) + 1
	case cc(t,s1,s2) => angryFace(s1) + angryFace(s2)
}

val myS:S = bc(a) // bac
val myN:Int = 1
assert(angryFace(myS) == myN)


def happyFace(s: S): Int = s match {
	case `a` => 0 // on the exam you can write this without the backtic
	case bc(s1) => {
		val n1 = happyFace(s1)
		val np = n1 + 1
		np
	}
	case cc(t,s1,s2) => {
		happyFace(s1) + happyFace(s2) + 2
	}
}

assert(happyFace(cc(Nil,a,a))==2)








