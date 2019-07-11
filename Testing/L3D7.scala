sealed abstract class LL[+A]
case object End extends LL
case class Node[A](h:A,t:LL[A]) extends LL[A]

val my:LL[String] = Node("Bonjour", Node("Nemo", Node(",",Node("ca"),Node("va",Node("?",End)))))

def foldLeftOnLL[A,B](l:LL[A])(z:B)(f:(B,A)=>B):B = {
  def loop(l:LL[A],acc:B):B = l match {
    case End => z
    case Node(h,t) => f(loop(t,acc),h)
  }
  loop(l,z)
}

def printLL[a](l:LL[A]):Unit = println(
  foldLeftOnLL(l)("Nil")((acc,h)=>h+"->"+acc))
  printLL(myL)
  )