/*
Ask me how to do easy things in scala... then we'll move on to the harder stuff
*/

def foo(n:Int):Unit = {
  if (n > 1) {
    println(n)
    foo(n - 1)
  }
  else {
    println(n)
  }
}

//foo(100)

def bar (n: Int): Unit = {
  if (n < 100) {
    println(n)
    bar(n + 1)
  }
  else println(n)
}
// bar(1)



def foo2(n:Int):Unit = {
  if (n > 1) {
    foo2(n - 1)
    println(n)
  }
  else {
    println(n)
  }
}

// foo2(100)



sealed abstract class LList
case object Tail extends LList
case class Node(data:Int, next:LList) extends LList

val l0:LList = Tail
val l1:LList = Node(2, Tail)
val l1Alt:LList = Node(2, l0)

var x:Int = 2
x = 3

val y:Int = 2
//y = 3  throw error

def append(l:LList, n:Int):LList = l match {
  case Tail => Node(n, Tail)
  case Node(d,next) => {
    val nextp:LList = append(next, n)
    val lp:LList = Node(d, nextp)
    lp
  }
  //Node(d, append(next,n))
}

if (l1 == append(l0, 2)) println("YATTA")













