  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match {
    case Nil => Nil
    case h :: t => f(h) match {
      case None => {
        val t2 = mapFirst(t)(f)
        h :: t2
      }
      case Some(h2) => h2 :: t
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Binary(bop, e1, e2) => Binary(bop,subst(e1),subst(e2))
    }
    subst(e)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => !isValue(e)
    case MName => false
  }

  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      case Call(function@Function(None,params,_,eFuncBody),args) => {
        val paramsArgsZip = params zip args
        val allNotRedex:Boolean = paramsArgsZip.forall{
          case (((xi,MTyp(modei,typei)),argi)) => ! isRedex(modei,argi)
        }
//        val atLeast1IsReducible:Boolean = {
//          paramsArgsZip.exists{
//            case ((xi,MTyp(modei,typei)),argi) => isRedex(modei,argi)
//          }
//        }
//        val allNotRedex:Boolean = ! atLeast1IsReducible

        if (allNotRedex) { // DoCall
          // HOF on Lists
          // substitue on function body a bunch of times
          // sub argument for parameter name in function body
          val newFuncBody = paramsArgsZip.foldLeft(eFuncBody){
            case ((eFuncBodyi, ((parami,_),argi))) => {
              substitute(eFuncBodyi,argi,parami)
            }
          }
          newFuncBody
        }
        else {
          // SearchCall2
          val params_argsp_zip = mapFirst(paramsArgsZip){
            case ((parami@(_,MTyp(modei,_)),argi)) if isRedex(modei, argi) => {
              val argip:Expr = step(argi)
              Some((parami,argip))
            }
            case _ => None
          }
          val argsp = params_argsp_zip map { case ((pi,ai)) => ai }
          Call(function, argsp)
        }
      }
        // SearchCall2 or DoCall
      case Call(Function(None,params,_,eFuncBody),args) => {
        // [0,1,2] zip ['a','b','c']
        // [(0,'a'),(1,'b'),(2,'c')]
        val paramsArgsZip = params zip args
//        val paramsArgsZip = params.zip(args)

        // exists   : Boolean
        // are any arguments reducible
        val atLeast1IsReducible:Boolean = {
//          paramsArgsZip exists { case ((_,MTyp(modei,_)),argi) => isRedex(modei,argi) }

          paramsArgsZip.exists({
              case ((xi,MTyp(modei,typei)),argi) => isRedex(modei,argi)
              case (("hello there",_),_) => {"this won't happen"; true}
          })

//          paramsArgsZip.exists({
//            (paramArgi) => paramArgi match {
//              case ((xi,MTyp(modei,typei)),argi) => isRedex(modei,argi)
//              case _ => ???
//            }
//          })

//          paramsArgsZip.exists({
//            (paramArgi) => {
//              val parami:(String, MTyp) = paramArgi._1
//              val modeTypei:MTyp = parami._2
//              val MTyp(modei,_) = modeTypei
//              val argi = paramArgi._2
//              if (isRedex(modei,argi)) { true } else { false }
//            }
//          })
        }


        val allNotRedex:Boolean = ! atLeast1IsReducible


        if (allNotRedex) {
          // DoCall
          ???
        }
        else {
          // SearchCall2
          ???
        }
      }

      case Call(v1, args) if isValue(v1) =>
        v1 match {
          case Function(p, params, _, e1) => {
            // [0,1,2] zip ['a','b','c']
            // [(0,'a'),(1,'b'),(2,'c')]
            val pazip = params zip args
            if (???) {
              val e1p = pazip.foldRight(e1) {
                ???
              }
              p match {
                case None => ???
                case Some(x1) => ???
              }
            }
            else {
              // SearchCall2
              val pazipp = mapFirst(pazip) {
                ???
              }
              ???
            }
          }
          case _ => throw StuckError(e)
        }
    }
  }


  /* External Interfaces */
//  this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}

