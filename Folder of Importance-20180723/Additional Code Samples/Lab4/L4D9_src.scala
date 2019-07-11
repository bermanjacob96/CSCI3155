package jsy.student

import jsy.lab4.Lab4Like

object Lab4 extends jsy.util.JsyApplication with Lab4Like {
  import jsy.lab4.ast._
  import jsy.lab4.Parser

  /*
   * CSCI 3155: Lab 4
   * <Your Name>
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /* Collections and Higher-Order Functions */

  /* Lists */

  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    //    case Nil => Nil
    //    case h :: Nil => h :: Nil
    case h1 :: (t1 @ (h2 :: t2)) => {
      if (h1 == h2) {
        compressRec(t1)
      } else {
        h1 :: compressRec(t1)
      }
    }
    //    case h1 :: h2 :: t2 => {
    //      if (h1 == h2) {
    //        compressRec(h2::t2)
    //      } else {
    //        h1 :: compressRec(h2::t2)
    //      }
    //    }
  }

  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h:A, acc:List[A]) => ???
  }

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

  /* Trees */
  def foldLeftOnList[A,B](l:List[A])(z:B)(f:(B,A)=>B): B = {
    l match {
      case Nil => z
      case h::t => {
        val zp = f(z,h)
        foldLeftOnList(t)(zp)(f)
      }
    }
  }

  def foldLeft[A](t: Tree)(z: A)(f: (A, Int) => A): A = {
    def loop(acc: A, t: Tree): A = t match {
      case Empty => acc
      case Node(l, d, r) => {
        val accp = loop(acc, l)
        val accpp = f(accp,d)
        val accppp = loop(accpp,r)
        accppp
//        loop(f(loop(acc,l),d),r)
      }
    }
    loop(z, t)
  }

  // An example use of foldLeft
  def sum(t: Tree): Int = foldLeft(t)(0){ (acc, d) => acc + d }

  // Create a tree from a list. An example use of the
  // List.foldLeft method.
  def treeFromList(l: List[Int]): Tree =
  l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

//  def strictlyOrdered(t: Tree): Boolean =
//    foldLeft(t)(Some(None): Option[Option[Int]]) {
//      // only hits on left most data point
//      // which should be the min of the tree
//      case (Some(None),minOfTree) => Some(Some(minOfTree))
//      case (Some(Some(min)), d) if min < d => Some(Some(min))
//      case _ => None
//    }.isDefined
//
//  def foo[A](ooA:Option[Option[A]])(cb:Option[A] => B):Option[Option[B]] = {
//    ooA match {
//      case None => None
//      case Some(oA) => Some(Some(cb(oA)))
//    }
//  }

  def strictlyOrdered(t: Tree): Boolean = {
    val (b,_) =foldLeft(t)((true,None):(Boolean,Option[Int])) {
      case ((true,None),leftMostData) => (true, Some(leftMostData))
      case ((true,Some(previousData)),data) => (previousData < data, Some(data))
      case ((false,_),_) => (false, None)
    }
    b
  }

//    val (b, _) = foldLeft(t)((true, None: Option[Int])){
//      case ((true,None),d) => (true,Some(d))
//      case ((true,Some(min)),d) => ???
//      case ((false,None),d) => ???
//      case ((false,Some(dPrev)),d) => ???
//    }
//    b
//  }

  /* Type Inference */

  // While this helper function is completely given, this function is
  // worth studying to see how library methods are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)
    def ty(e:Expr):Typ = typeof(env,e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => lookup(env,x)
      case Decl(mode, x, e1, e2) => typeof(env, e1) match {
        case t1 => {
          val envSpecial:TEnv = extend(env, x, t1)
          typeof(envSpecial, e2) match {
            case t2 => t2
          }
        }
      }

      case Obj(keyVals) => TObj(keyVals map { kv => { val k = kv._1; ??? } })
      case Obj(keyVals) => TObj(keyVals map { case ((k,v)) =>  ??? })

      case Obj(keyVals) => TObj(keyVals mapValues ty)

      case Obj(keyVals) => {
        val keyTypes = keyVals mapValues ty
        TObj(keyTypes)
      }

      case Obj(keyVals) => {
        val keyTypes = keyVals mapValues { (ei) => typeof(env,ei) }
        TObj(keyTypes)
      }

      case Obj(keyVals:Map[String,Expr]) => {
        val keyTypes:Map[String,Typ] = {
          keyVals mapValues {
            (ei:Expr) => {
              val ti:Typ = typeof(env,ei)
              ti
            }
          }
        }
        TObj(keyTypes)
      }



      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case t1 => err(t1, e1)
      }
      case Binary(Plus,e1,e2) => ty(e1) match {
        case t1@(TString|TNumber) => {
          val t2 = ty(e2)
          if (t1==t2) { t1 } else { err(t2,e2) }
        }
        case tgot => err(tgot,e1)
      }
//      case Binary(Plus,e1,e2) => typeof(env,e1) match {
//        case TNumber => typeof(env,e2) match {
//          case TNumber => TNumber
//          case tgot => err(tgot,e2)
//        }
//        case TString => typeof(env,e2) match {
//          case TString => TString
//          case tgot => err(tgot,e2)
//        }
//        case tgot => err(tgot,e1)
//      }
//      case Binary(Plus, e1, e2) => {
//        val t1 = typeof(env,e1)
//        val t2 = typeof(env,e2)
//        (t1,t2) match {
//          case (TNumber,TNumber) => TNumber
//          case (TString,TString) => TString
//          case (TNumber,tgot) => err(tgot,e2)
//          case (TString,tgot) => err(tgot,e2)
//          case (tgot,_) => err(tgot,e1)
//        }
//      }
      case Binary(Minus|Times|Div, e1, e2) => typeof(env,e1) match {
        case TNumber => typeof(env,e2) match {
          case TNumber => TNumber
          case t2 => err(t2,e2)
        }
        case otherType => err(otherType, e1)
      }
      case Binary(Eq|Ne, e1, e2) =>
        ???
      case Binary(Lt|Le|Gt|Ge, e1, e2) =>
        ???
      case Binary(And|Or, e1, e2) =>
        ???
      case Binary(Seq, e1, e2) =>
        ???
        // typeIf
      case If(e1, e2, e3) => typeof(env,e1) match {
        case TBool => {
          val t2 = typeof(env,e2)
          val t3 = typeof(env,e3)
          if (t2 == t3) { t2 } else { err(t3,e3) }
        }
        case tgot => err(tgot,e1)
      }
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          /***** Add cases here *****/
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = ???
        // Infer the type of the function body
        val t1 = ???
        // Check with the possibly annotated return type
        ???; TFunction(???,???)
      }
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          // List[A].foreach{(A) => Unit}
          (params zip args).foreach {
            case ((x,MTyp(m,t)),e) => ???
            case _ => { println("WAT"); ??? }
            //            paz => paz match {
//              case ((x,MTyp(m,t)),e) => ???
////              case (p:(String,MTyp)@(x:String MTyp(m:Mode,t:Typ)),a:Expr) =>
//            }
          };
          tret
        case tgot => err(tgot, e1)
      }
      case Obj(fields:Map[String,Expr]) => {
        val tFields:Map[String,Typ] = ???
        TObj(tFields)
      }
      case GetField(e1, f) => ???
    }
  }


  /* Small-Step Interpreter */

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case _ => ??? // delete this line when done
    }
  }

  /* This should be the same code as from Lab 3 */
  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = ???
    loop(e0, 0)
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      /***** Cases from Lab 3 */
      case Unary(uop:Uop, e1:Expr) => ???
      case Binary(bop, e1, e2) => Binary(bop,subst(e1),subst(e2))
      case If(e1, e2, e3) => ???
      case Var(y) => esub
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

//        val fvs = freeVars(esub)
//        def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
//        subst(rename(e)(fresh))
    subst(e)
  }


  /* Rename bound variables in e */
  def rename(e: Expr)(fresh: String => String): Expr = {
    def ren(env:Map[String,String], e: Expr): Expr = {
      e match {
        case N(_) | B(_) | Undefined | S(_) => e
        case Print(e1) => Print(ren(env, e1))

        case Unary(uop, e1) => ???
        case Binary(bop, e1, e2) => ???
        case If(e1, e2, e3) => ???

        case Var(y) =>
          ???
        case Decl(mode, y, e1, e2) =>
          val yp = fresh(y)
          ???

        case Function(p, params, retty, e1) => {
          val (pp, envp): (Option[String], Map[String,String]) = p match {
            case None => ???
            case Some(x) => ???
          }
          val (paramsp, envpp) = params.foldRight( (Nil: List[(String,MTyp)], envp) ) {
            ???
          }
          ???
        }

        case Call(e1, args) => ???

        case Obj(fields) => ???
        case GetField(e1, f) => ???
      }
    }
    ren(empty, e)
  }

  // Check if the expression needs to be reduced further
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst if !isValue(e) => true
    case _ => false
  }

  def isNotRedex(m:Mode,e:Expr):Boolean =  !isRedex(m,e)

  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      // DoDecl
      case Decl(m, x, eBind, eUse) => {
        if ( !isRedex(m, eBind) ) {
          substitute(eUse, eBind, x)
        }
        else {
          Decl(m, x, step(eBind), eUse)
        }
      }

        /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

        // DoAndTrue:   true && e2 --> e2
      case Binary(And,B(true),e2) => e2


      /***** Cases needing adapting from Lab 3. */
      case Unary(Neg,N(n1)) => N(-n1)
      //      case Unary(Neg, v1) if isValue(v1) => ???
      /***** More cases here */
      // SearchCall2 or DoCall
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
      case Call(Function(None,params,_,eBody),args) => {
        val modes:List[Mode] = params map { case xmti@(xi,MTyp(mi,ti)) => mi }
        val modesArgs:List[(Mode,Expr)] = modes zip args
        val allArgsNotFurtherRedex:Boolean = {
          modesArgs forall { case modeargi@(modei, argi) => isNotRedex(modei,argi) }
        }
        if ( allArgsNotFurtherRedex ) {
          // DoCall
          ???
        }
        else {
          // SearchCall2
          ???
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
      /***** New cases for Lab 4. */
        // DoArith
      case Binary(Minus,N(n1),N(n2)) => {
        val np:Double = n1 - n2
        N(np)
      }

      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      /***** Cases from Lab 3. */
      case Unary(uop, e1) => ???
      /***** More cases here */
      /***** Cases needing adapting from Lab 3 */
      case Call(v1 @ Function(_, _, _, _), args) => ???
      case Call(e1, args) => ???
      /***** New cases for Lab 4. */

      /* Everything else is a stuck error. Should not happen if e is well-typed.
       *
       * Tip: you might want to first develop by comment out the following line to see which
       * cases you have missing. You then uncomment this line when you are sure all the cases
       * that you have left the ones that should be stuck.
       */
      case _ => throw StuckError(e)
    }
  }


  /* External Interfaces */
//  this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}

