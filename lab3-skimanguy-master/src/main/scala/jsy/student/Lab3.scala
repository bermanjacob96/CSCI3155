package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._

  /*
   * CSCI 3155: Lab 3
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
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */

  override type Env = Map[String, Expr]
  val emp: Env = Map()

  override def lookup(env: Env, x: String): Expr = env(x)

  override def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0.0
      case B(true) => 1.0
      case Undefined => Double.NaN
      case S(s) => try s.toDouble catch {
        case _: Throwable => Double.NaN
      }
      case Function(_, _, _) => Double.NaN
      case _ => throw new UnsupportedOperationException
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(0.0) => false //0.0 is false, any other number is true
      case N(n) => {
        if (n.isNaN) false else true
      } //except NaN
      case S(s) => if (s == "") false else true //empty strings are false (0.0) all other strings are true (non zero)
      case Undefined => false
      case Function(_, _, _) => true
      case _ => throw new UnsupportedOperationException
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => prettyNumber(n) //this checks if it is NaN, whole number, etc
      case B(b) => if (b) "true" else "false"
      // Here in toStr(Function(_, _, _)), we will deviate from Node.js that returns the concrete syntax
      // of the function (from the input program).
      case Function(_, _, _) => "function"
      case _ => throw new UnsupportedOperationException
    }
  }

  /*
 * Helper function that implements the semantics of inequality
 * operators Lt, Le, Gt, and Ge on values.
 *
 * We suggest a refactoring of code from Lab 2 to be able to
 * use this helper function in eval and step.
 */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case _ => ??? // delete this line when done
    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */

  /*
 * Start by copying your code from Lab 2 here.
 */
  def eval(env: Env, e: Expr): Expr = {
    def eToN(e: Expr): Double = toNumber(eval(env, e))
    def eToB(e: Expr): Boolean = toBoolean(eval(env, e))
    def eToVal(e: Expr): Expr = eval(env, e)
    e match {
      /* Base Cases */
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Var(x) => lookup(env, x)

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      // ****** Your cases here
      case ConstDecl(x, e1, e2) => {
        val v1 = eToVal(e1)
        val new_env = extend(env, x, v1)
        eval(new_env, e2)
      }

      case If(e1, e2, e3) =>
        val cond = eToB(e1)
        if (cond) eToVal(e2) else eToVal(e3)


      case Unary(uop, e1) => uop match {
        case Not => B(!toBoolean(eToVal(e1)))
        case Neg => N(-toNumber(eToVal(e1)))
        case _ => throw new UnsupportedOperationException
      }

      case Call(e1, e2) => {
        val v1 = eToVal(e1)
        val v2 = eToVal(e2)
        (v1: @unchecked) match {
          case (Function(None, param, body)) => {
            val new_env = extend(env, param, v2)
            eval(new_env, body)
          }
          case (Function(Some(name), param, body)) => {
            val name_bind = extend(env, name, v1)
            val new_env = extend(name_bind, param, v2)
            eval(new_env, body)
          }
          case _ => throw new DynamicTypeError(e1)
        }
      }


      case Binary(bop, e1, e2) => bop match {

        //Right associative, recursive down the right
        case Seq => {
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          v2 //Seq only returns the second expression
        }

        case Plus => {
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1, v2) match {
            case (Function(_,_,_), _) => throw new DynamicTypeError(e)
            case (_, Function(_,_,_)) => throw new DynamicTypeError(e)
            case (S(s), v2) => S(s + toStr(v2))
            case (v1, S(s)) => S(toStr(v1) + s)
            case (_, _) => N(toNumber(v1) + toNumber(v2))
          }
        }

        case Ne => {
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1, v2) match {
            case (Function(name, param, body), _) => throw new DynamicTypeError(e)
            case (_, Function(name, param, body)) => throw new DynamicTypeError(e)
            case (v1, v2) => B(v1 != v2)
          }
        }

        case Eq => {
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1, v2) match {
            case (Function(name, param, body), _) => throw new DynamicTypeError(e)
            case (_, Function(name, param, body)) => throw new DynamicTypeError(e)
            case (v1, v2) => B(v1 == v2)
          }
        }

        case Minus => N(toNumber(eToVal(e1)) - toNumber(eToVal(e2)))
        case Times => N(toNumber(eToVal(e1)) * toNumber(eToVal(e2)))
        case Div => N(toNumber(eToVal(e1)) / toNumber(eToVal(e2)))

        //Comparison Operators
        case Gt => {
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          //evaluate each to match the values
          (v1, v2) match {
            //In case of strings
            case (S(s1), S(s2)) => B((s1) > (s2)) //really looking at length
            case (S(s1), _) => {
              if (s1 == "") B(0 > toNumber(v2)) //if it is an empty string set it to 0 and check
              else B(toNumber(eToVal(v1)) > toNumber(eToVal(v2)))
            }
            //if the second is a string
            case (_, S(s2)) => {
              //if it is an empty string evaluate that the other number is larger than 0
              if (s2 == "") B(toNumber(v1) > 0)
              else B(toNumber(eToVal(v1)) > toNumber(eToVal(v2)))
            }
            case (_, _) => B(toNumber(eToVal(v1)) > toNumber(eToVal(v2)))
          }
        }

        case Ge => {
          //look at comments for Gt for explanation
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1, v2) match {
            case (S(s1), S(s2)) => B((s1) >= (s2))
            case (S(s1), _) => {
              if (s1 == "") B(0 >= toNumber(v2))
              else B(toNumber(eToVal(v1)) >= toNumber(eToVal(v2)))
            }
            case (_, S(s2)) => {
              if (s2 == "") B(toNumber(v1) >= 0)
              else B(toNumber(eToVal(v1)) >= toNumber(eToVal(v2)))
            }
            case (_, _) => B(toNumber(eToVal(v1)) >= toNumber(eToVal(v2)))
          }
        }

        //Less than
        case Lt => {
          //look at comments for Gt for explanation
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1, v2) match {
            case (S(s1), S(s2)) => B((s1) < (s2))
            case (S(s1), _) => {
              if (s1 == "") B(0 < toNumber(v2))
              else B(toNumber(eToVal(v1)) < toNumber(eToVal(v2)))
            }
            case (_, S(s2)) => {
              if (s2 == "") B(toNumber(v1) < 0)
              else B(toNumber(eToVal(v1)) < toNumber(eToVal(v2)))
            }
            case (_, _) => B(toNumber(eToVal(v1)) < toNumber(eToVal(v2)))
          }
        }

        //Less than or Equal to
        case Le => {
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1, v2) match {
            case (S(s1), S(s2)) => B((s1) <= (s2))
            case (S(s1), _) => {
              //evaluate empty strings to 0, then check
              if (s1 == "") B(0 <= toNumber(v2))
              else B(toNumber(eToVal(v1)) <= toNumber(eToVal(v2)))
            }
            case (_, S(s2)) => {
              if (s2 == "") B(toNumber(v1) <= 0)
              else B(toNumber(eToVal(v1)) <= toNumber(eToVal(v2)))
            }
            case (_, _) => B(toNumber(eToVal(v1)) <= toNumber(eToVal(v2)))
          }
        }
        case And => {
          val v1 = eToVal(e1)
          if (toBoolean(v1)) eToVal(e2)
          else v1
        }

        //If the first is true, return true, otherwise evaluate the second
        case Or => {
          val v1 = eToVal(e1)
          if (toBoolean(v1)) v1
          else eToVal(e2)
        }

        case _ => throw new UnsupportedOperationException
      } //end BOP match
      case _ => throw new UnsupportedOperationException
    }
  }


  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = {
    }
    loop(e0, 0)
  }


  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    /* Simple helper that calls substitute on an expression
     * with the input value v and variable name x. */
    def subst(e: Expr): Expr = substitute(e, v, x)
    /* Body */
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Var(y) => if (y == x) v else e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Call(e1, e2) => Call(subst(e1), subst(e2))
      case Function(None, param, body) => if (param == x) e else Function(None, param, subst(body))
      case Function(Some(name), param, body) => {
        if (param == x) e
        else if (name == x) e
        else Function(Some(name), param, subst(body))
      }
      case ConstDecl(y, e1, e2) => {
        if (y == x) ConstDecl(y, subst(e1), e2)
        else ConstDecl(y, subst(e1), subst(e2))
      }
    }
  }

  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

      //**DoNeg**
      case Unary(Neg, v1) if isValue(v1) => N(-1*toNumber(v1))

      //**DoNot**
      case Unary(Not, v1) if isValue(v1) => B(!toBoolean(v1))

      //**DoSeq**
      case Binary(Seq, v1, e2)  if isValue(v1) => e2

      //**DoPlus**

      case Binary(Plus, e1, e2) if isValue(e1) && isValue(e2) => (e1, e2) match{
        case (S(s1), v2) => S(s1 + toStr(v2))
        case (v1, S(s2)) => S(toStr(v1) +s2)
        case (_,_) => N(toNumber(e1)+toNumber(e2))
      }

      //**DoArith**
      case Binary(Minus, v1, v2) if isValue(v1) && isValue(v2) => N(toNumber(v1) - toNumber(v2))
      case Binary(Times, v1, v2) if isValue(v1) && isValue(v2) => N(toNumber(v1) * toNumber(v2))
      case Binary(Div, v1, v2) if isValue(v1) && isValue(v2) => N(toNumber(v1) / toNumber(v2))


      //**DoInequality**
      case Binary(Gt, v1, v2)  if isValue(v1) && isValue(v2) =>
        (v1, v2) match {
          case (S(s1), S(s2)) => B(s1 > s2)
          case (_, _) => B(toNumber(v1) > toNumber(v2))
        }
      case Binary(Ge, v1, v2)  if isValue(v1) && isValue(v2) =>
        (v1, v2) match {
          case (S(s1), S(s2)) => B(s1 >= s2)
          case (_, _) => B(toNumber(v1) >= toNumber(v2))
        }
      case Binary(Lt, v1, v2)  if isValue(v1) && isValue(v2) =>
        (v1, v2) match {
          case (S(s1), S(s2)) => B(s1 < s2)
          case (_, _) => B(toNumber(v1) < toNumber(v2))
        }
      case Binary(Le, v1, v2)  if isValue(v1) && isValue(v2) =>
        (v1, v2) match {
          case (S(s1), S(s2)) => B(s1 <= s2)
          case (_, _) => B(toNumber(v1) <= toNumber(v2))
        }

      //**DoEquality**
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) =>
        (v1, v2) match {
          case (Function(_, _, _), _) => throw new DynamicTypeError(e)
          case (_, Function(_, _, _)) => throw new DynamicTypeError(e)
          case (_, _) => B(v1 == v2)
        }

      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) =>
        (v1, v2) match {
          case (Function(_, _, _), _) => throw new DynamicTypeError(e)
          case (_, Function(_, _, _)) => throw new DynamicTypeError(e)
          case (_, _) => B(v1 != v2)
        }

      //**DoAnd**
      case Binary(And, B(true), e2) => e2
      case Binary(And, B(false), e2) => B(false)
      case Binary(And, v1, e2) if isValue(v1) => if (toBoolean(v1)) e2 else v1

      //**DoOr**
      case Binary(Or, B(true), e2) => B(true)
      case Binary(Or, B(false), e2) => e2
      case Binary(Or, v1, e2) if isValue(v1) => if (toBoolean(v1)) v1 else e2

      //**DoIf**
      case If(B(true), e2, e3) => e2
      case If(B(false), e2, e3) => e3
      case If(v1, e2, e3) if isValue(v1) => if (toBoolean(v1)) e2 else e3

      //**DoConst**
      case ConstDecl(x, v1, e2) if isValue(v1) => substitute(e2, v1, x)

      //**DoCall**
      case Call(v1, v2) if isValue(v1) && isValue(v2) => v1 match {
        case Function(None, param, body) => substitute(body, v2, param)
        case Function(Some(name), param, body) => {
          val new_body = substitute(body, v1, name)
          substitute(new_body, v2, param)
        }
        case N(n) => throw new DynamicTypeError(e)
      }


      ////////////////////////////////////
      /* Inductive Cases: Search Rules */

      //**SearchPrint**
      case Print(e1) => Print(step(e1))

      //**SearchUnary**
      case Unary(uop, e1) => Unary(uop, step(e1))

      //**SearchBinary**
      case Binary(bop @ (Plus | Minus | Times | Div | Lt | Le | Gt | Ge), v1, e2) if isValue(v1) => Binary(bop, v1, step(e2))

      case Binary(bop @ (Eq | Ne), v1, e2) if isValue(v1) => (v1, e2) match {
        case (Function(_, _, _), e2) => throw new DynamicTypeError(e)
        case (v1, e2) => Binary(bop, v1, step(e2))
      }

      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)

      //**SearchIf**
      case If(e1, e2, e3) => If(step(e1), e2, e3)

      //**SearchConst**
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)

      //**SearchCall**
      case Call(e1@Function(_, _, _), e2) => Call(e1, step(e2))
      case Call(v1, e2) if isValue(v1) => throw new DynamicTypeError(e)
      case Call(e1, e2) => Call(step(e1), e2)


      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }

  /* External Interfaces */

  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}
