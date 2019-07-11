package jsy.student

import jsy.lab4.Lab4Like
import jsy.lab4.ast._
import jsy.tester.JavascriptyTester
import org.scalatest._

class Lab4Spec(lab4: Lab4Like) extends FlatSpec {
  import lab4._

  /***** Higher-Function Exercises Tests *****/

  "compressRec/compressFold" should "compress List(1, 2, 2, 3, 3, 3)" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) {
      compressRec(l1)
    }
  }
  it should "bleh" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) { compressFold(l1) }
  } 
  
  "mapFirst" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     assertResult(gold1) {
       mapFirst(l1) { (i: Int) => if (i < 0) Some(-i) else None }
     }
  }
  
  "foldLeft" should "enable implementing treeFromList and sum" in {
    assertResult(6){
      sum(treeFromList(List(1, 2, 3)))
    }
  }

  "strictlyOrdered" should "check strict ordering of a binary search tree" in {
    assert(!strictlyOrdered(treeFromList(List(1,1,2))))
    assert(strictlyOrdered(treeFromList(List(1,2))))
  }


  /***** Interpreter Tests *****/

  {
    val xtype = TNumber
    val tenvx = extend(empty, "x", xtype)

    "TypeVar" should "perform TypeVar" in {
      assertResult(xtype) {
        typeof(tenvx, Var("x"))
      }
    }

    // Probably want to write some more tests for typeInfer, substitute, and step.

  }

}


// BEGIN typeOf tests
class Lab4TypeOfSpec(lab4: Lab4Like) extends FlatSpec {
  import lab4._
  import jsy.lab4.Parser.parse
  // I'm going to comment these out for now and bring them back when they are meaningful tests
//   "TypeVar" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeNeg" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeNot" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeSeq" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeArith" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypePlusString" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeInequalityNumber" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeInequalityString" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeEquality" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeAndOr" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypePrint" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeIf" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeDecl" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeCall" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeObject" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeGetField" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeNumber" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeBool" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeString" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeUndefined" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeFunction" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeFuncationAnn" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   "TypeRecFunction" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
//   it should "fail in base case" in {
//      // not sure how to capture this test
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val tExpected:Typ = TUndefined
//      assertResult(tExpected) {
//         typeof(empty, e)
//      }
//   }
}

class Lab4SpecTypeOfRunner extends Lab4TypeOfSpec(jsy.student.Lab4)




// BEGIN step tests
class Lab4StepSpec(lab4: Lab4Like) extends FlatSpec {
  import lab4._
  import jsy.lab4.Parser.parse
//   "DoNeg" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
   "DoNot" should "succeed in base case" in {
      val e:Expr = parse("!true")
      val ep:Expr = parse("false")
      assertResult(ep) { 
         step(e)
      }
   }
   it should "also succeed in inductive cases" in {
      val e:Expr = parse("!!true")
      val ep:Expr = parse("!false")
      val epp:Expr = parse("true")
      assertResult(ep) { 
         step(e)
      }
     assertResult(epp) { step(ep) }
   }
//   "DoSeq" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoArith" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoPlusString" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoInequalityNumber" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoInequalityString" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoEquality" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoAndTrue" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoAndFalse" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoOrTrue" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoOrFalse" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoPrint" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoIfTrue" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoIfFalse" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoDecl" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoCall" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoCallRec" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "DoGetField" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchUnary" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchBinary1" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchBinary2" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchPrint" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchIf" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchDecl" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchCall1" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchCall2" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchObject" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   "SearchGetField" should "succeed in base case" in {
//      val e:Expr = parse("1")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
//   it should "also succeed in inductive cases" in {
//      val e:Expr = parse("1 + 2")
//      val ep:Expr = parse("1")
//      assertResult(ep) {
//         step(e)
//      }
//   }
}

class Lab4SpecStepRunner extends Lab4StepSpec(jsy.student.Lab4)



// An adapter class to pass in your Lab4 object.
class Lab4SpecRunner extends Lab4Spec(jsy.student.Lab4)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab4.
// The test expects a corresponding .ans file with the expected result.
class Lab4JsyTests extends JavascriptyTester(None, "lab4", jsy.student.Lab4)

class Lab4Suite extends Suites(
  new Lab4SpecRunner,
  new Lab4SpecTypeOfRunner,
  new Lab4SpecStepRunner,
  new Lab4JsyTests
)
