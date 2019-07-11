sealed abstract class Expr
case class N(n:Double) extends Expr
case class Binary(bop: Bop, e1:Expr, e2: Expr) extends Expr
case class ConstDecl(ebind:Expr, ebody:Double => Expr ) extends Expr

sealed abstract class Bop
case object Plus extends Bop

def eval(e:Expr):Expr = e match {
	case N(n) => N(n)
	case Binary(Plus, e1, e2) => {
		val N(n1) = eval(e1)
		val N(n2) = eval(e2)
		N(n1 + n2)
	}
	case ConstDecl(ebind, ebody) => {
		val N(n) = eval(ebind)
		val ebodyp = ebody(n)
		eval(ebodyp)
	}
}
assert(N(3) == eval(ConstDecl(N(1), (x:Double) => Binary(Plus, N(x), N(2)))))