 /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      /***** Cases from Lab 3 */
      case Unary(uop:Uop, e1:Expr) => ???
      case Binary(bop, e1, e2) => ???
      case If(e1, e2, e3) => ???
      case Var(y) => ???
      case Decl(mode, y, e1, e2) => {
        // was it shaddowed?
        if (x == y) {
          val e1Sub = subst(e1)
          Decl(mode,y,e1Sub,e2)
        }
        else {
          val e1Sub = subst(e1)
          val e2Sub = subst(e2)
          Decl(mode,y,e1Sub,e2Sub)
        }
      }
      /***** Cases needing adapting from Lab 3 */
      case Function(p, params, tann, eFuncBody) => p match {
        case None => {
          val b = params exists { case ((xCur,_)) => xCur == x }
          if (b) {
            // it has been shaddowed
            Function(p,params,tann,eFuncBody)// e
          }
          else {
            val foo = subst(eFuncBody)
            Function(None,params,tann,foo)
          }
        }
        case Some(funcName) => {
          ???
        }
      }

      case Function(p, params, tann, eFuncBody) => p match {
        case _ if params exists { case ((xCur,_)) => xCur == x } => e
        case Some(funcName) if false => e // change from false
        case _ => {
          val foo = subst(eFuncBody)
          Function(None,params,tann,foo)
        }
      }
      case Function(Some(funcName), params, tann, eFuncBody) => ???

        case Call(e1, args) => ???
      /***** New cases for Lab 4 */
      case Obj(fields) => ???
      case GetField(e1, f) => ???
    }

    //    val fvs = freeVars(???)
    //    def fresh(x: String): String = if (???) fresh(x + "$") else x
    //    subst(???)
    subst(e)
  }