package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
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
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */

  //ScalaMap[String,Expr]
  override type Env = Map[String, Expr]
  override val empty: Env = Map()
  override def lookup(env: Env, x: String): Expr = env(x)
  override def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }


  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if (b) 1 else 0
      case S(s) => try s.toDouble catch {case _: Throwable => Double.NaN}
      case Undefined => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(0) => false
      case N(n) => if (n.isNaN()) false else true
      case S("") => false
      case S(_) => true
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case B(b) => if (b) "true" else "false"
      case N(n) => n.toString
      case Undefined => "undefined"
    }
  }

  def eval(env: Env, e: Expr): Expr = {

    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */

      case B(b) => B(b)
      case N(n) => N(n)
      case S(s) => S(s)
      case Var(x) => lookup(env, x)
      case Unary(uop, e1) => uop match {
        case Not => B(!toBoolean(eToVal(e1)))
        case Neg => N(-toNumber(eToVal(e1)))
      }
      // If the conditional evaluates to true, evaluate first expression, otherwise evaluate the second expression.
      case If(e1, e2, e3) => if (toBoolean(eToVal(e1))) eToVal(e2) else eToVal(e3)
      case ConstDecl(x, e1, e2) => eval(extend(env,x,eToVal(e1)), e2)

      ////////
      ////////
      case Binary(bop, e1, e2) => bop match {

        case Plus => (eToVal(e1), eToVal(e2)) match {
          case (_, S(_)) => S(toStr(eToVal(e1)) + toStr(eToVal(e2)))
          case (S(_), _) => S(toStr(eToVal(e1)) + toStr(eToVal(e2)))
          case (_, _) => N(toNumber(eToVal(e1)) + toNumber(eToVal(e2)))
        }

        case Minus => N(toNumber(eToVal(e1)) - toNumber(eToVal(e2)))
        case Times => N(toNumber(eToVal(e1)) * toNumber(eToVal(e2)))
        case Div => N(toNumber(eToVal(e1)) / toNumber(eToVal(e2)))
        case Ne => B(eToVal(e1) != eToVal(e2))
        case Eq => B(eToVal(e1) == eToVal(e2))
        case Le => (eToVal(e1), eToVal(e2)) match {

          case (S(_), S(_)) =>  B(toStr(eToVal(e1)) <= toStr(eToVal(e2)))
          case (_, _) =>  B(toNumber(eToVal(e1)) <= toNumber(eToVal(e2)))
        }
        case Lt => (eToVal(e1), eToVal(e2)) match{

          case (S(_), S(_)) =>  B(toStr(eToVal(e1)) < toStr(eToVal(e2)))
          case (_, _) =>  B(toNumber(eToVal(e1)) < toNumber(eToVal(e2)))
        }
        case Gt => (eToVal(e1), eToVal(e2)) match{

          case (S(_), S(_)) =>  B(toStr(eToVal(e1)) > toStr(eToVal(e2)))
          case (_, _) =>  B(toNumber(eToVal(e1)) > toNumber(eToVal(e2)))
        }
        case Ge => (eToVal(e1), eToVal(e2)) match{

          case (S(_), S(_)) =>  B(toStr(eToVal(e1)) >= toStr(eToVal(e2)))
          case (_, _) =>  B(toNumber(eToVal(e1)) >= toNumber(eToVal(e2)))
        }

        case Or => if(toBoolean(e1))
          eToVal(e1) else eToVal(e2)

        case And => if(toBoolean(eToVal(e1)))
          eToVal(e2) else eToVal(e1)

        case Seq => eToVal(e1);eToVal(e2)

        case _ => throw new UnsupportedOperationException
      }

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case _ => e
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
