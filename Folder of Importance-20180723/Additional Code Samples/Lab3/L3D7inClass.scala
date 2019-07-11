sealed abstract class Stack[+A]
case object Bottom extends Stack
case class Elem[A](top:A,rest:Stack[A]) extends Stack[A]

def fold[A,B](s:Stack[A])(z:B)(f:(B,A)=>B):B = s match {
	case Bottom => z
	case Elem(t,r) => ???
}

val s:Stack[Int] = Elem(15,Elem(2,Elem(20,Bottom)))

def stackPrint[A](s:Stack[A]):Unit ={
	fold(s)(():Unit)((acc,h)=>println(h))
}
stackPrint(s)
val s2:Stack[String] = Elem("hi",Elem("low",Bottom))
stackPrint(s2)