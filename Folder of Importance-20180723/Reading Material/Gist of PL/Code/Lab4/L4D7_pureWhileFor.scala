 /*
  * @file 		L4D7_pureWhileFor.scala
  * @author 	Spencer D. Wilson
  * @date 		7/8/17
  *
  * @modifier	<your_name_here>
  * @date
  * 
  * @brief 
  * 	how can I create a useful while loop or for loop
  *   without mutable variables?
  *
  *   Type Object [M] denotes an abstraction of Memory
  *
  * @sources
  * 	https://github.com/csci3155/pppl-lab5
  *
  * @operate
  * 	scala <filepath>/L4D7_pureWhileFor.scala
  * 	
  */ 


/**************************************************
 *                                                * 
 *  DISCLAIMER :                                  *
 *    I don't know that this is entirely correct. *
 *                                                * 
 *************************************************/


/************
 *          * 
 *  PROMPT  *
 *          *
 ***********/
/* So, for a while now it has purplexed me... how can
we create a while loop or a for loop that is useful
without useing mutability? In most languages these 
constructs are only very useful when they modify the
state of memory.
*/
/* And then, it hit me... we are modifying the apperance
of memory. All I need is an abstraction of memory. I 
could do that with a state monad and I suppose that would
be ideal, but I could also create more light weight versions 
*/

// Many languages  init; while(cond){ <doStuff> } 

def whileLoop[M](init:M)(cond:(M)=>Boolean)(cb:(M)=>M):M = {
  if ( cond(init) ) {
    whileLoop(cb(init))(cond)(cb)
  } else {
    init
  }
}

// Many languages  for(init,cond,end_e){ <doStuff> } 
def forLoop[M](init:M)(cond:(M)=>Boolean)(end_e:(M)=>M)(cb:(M)=>M):M = {
  if ( cond(init) ) {
    forLoop(end_e(cb(init)))(cond)(end_e)(cb)
  } else {
    init
  }
}

/******************************
 *                            *
 *    TESTING                 *
 *                            *
 *****************************/


/*****************
*                *
*   Defining M   *
*                *
*****************/
/* Ideally I'd like memory to encapsolate a mapping
from variable names to a variety of expressions, 
each of which have a type and some of the types won't
be the same as eachother and some will be the same.
*/
/* For now, I will check that this works when all my 
types are the same. Later on I can build abstractions
over the expressions so that I can make it look like 
everything has the same type even if it doesn't.
*/

// I'll let my memory be abstracted as a Map[String,Int]
type Mem = Map[String,Int]
val m0:Mem = Map.empty
val m1:Mem = m0 + ("x" -> (1+1))
val m2:Mem = m1 + ("y" -> (10/2))

// toggle to check that mem is as expected... printing only...
def testMem(){
  println(m0)
  println(m1)
  println(m2)
}

/*********************
*                    *
*   Defining tests   *
*                    *
*********************/
def testAll() = {
  println("testing all=========================")
  testWhile()
  testFor()
}

def testWhile() = {
  println("testing While---------------------------")

  // defining subtests
  def test1() = {
    println("\nBegining test1")
    // x = 2 ; while(x<10){ println(x); x++ }; println("x has ~changed~ to:"+x)
    val my_init = m1
    def my_cond(m:Mem):Boolean = { m.getOrElse("x",???) < 10 }
    def my_cb(m:Mem):Mem = {
      println(m.getOrElse("x",???));
      m + ("x" -> (m.getOrElse("x",???)+1) )
    } 
    val mp = whileLoop(my_init)(my_cond)(my_cb)
    println("x has ~changed~ to:"+mp.getOrElse("x",???))
    println("End of test1 \n")
  }
  def test2() = {
    println("\nBegining test1")
    // x = 2; y = 5 ; while(x<10){ x++; y += 2 };
    val my_init = m2
    def my_cond(m:Mem):Boolean = { m.getOrElse("x",???) < 10 }
    def my_cb(m:Mem):Mem = {
      m + ("x" -> (m.getOrElse("x",???)+1) ) + ("y" -> (m.getOrElse("y",???)+2) )
    } 
    val mp = whileLoop(my_init)(my_cond)(my_cb)

    try {
      assert(mp("x")==2+10-2)
      assert(mp("y")==5+2*(10-2))
      println("\ttest 2 passed in testWhile")
    } catch {
      case _:Throwable => println("\ttest2 failed in testWhile")
    }
    println("End of test2 \n")
  }

  // calling subtests
  test1()
  test2()

}

def testFor() = {
  println("testing For---------------------------")

  // defining subtests
  def test1() = {
    println("\nBegining test1")
    // for(x=2;x<10;x++){ println(x); }; println("x has ~changed~ to:"+x)
    val my_init = m1
    def my_cond(m:Mem):Boolean = { m.getOrElse("x",???) < 10 }
    def my_end_e(m:Mem):Mem = { m + ("x" -> (m.getOrElse("x",???)+1) ) }
    def my_cb(m:Mem):Mem = {
      println(m.getOrElse("x",???));
      m
    } 
    val mp = forLoop(my_init)(my_cond)(my_end_e)(my_cb)
    println("x has ~changed~ to:"+mp.getOrElse("x",???))
    println("End of test1 \n")
  }
  def test2() = {
    println("\nBegining test1")
    // for(x = 2,y = 5 ;(x<10); x++ ){ y += 2 };
    val my_init = m2
    def my_cond(m:Mem):Boolean = { m.getOrElse("x",???) < 10 }
    def my_end_e(m:Mem):Mem = { m + ("x" -> (m.getOrElse("x",???)+1) ) }
    def my_cb(m:Mem):Mem = {
      m + ( "y" -> (m.getOrElse("y",???)+2) )
    } 
    val mp = forLoop(my_init)(my_cond)(my_end_e)(my_cb)

    try {
      assert(mp("x")==2+10-2)
      assert(mp("y")==5+2*(10-2))
      println("\ttest 2 passed in testFor")
    } catch {
      case _:Throwable => println("\ttest2 failed in testFor")
    }
    println("End of test2 \n")
  }

  // calling subtests
  test1()
  test2()
}

/*********************
*                    *
*   Defining tests   *
*                    *
*********************/
testAll()
// testMem() // print testing over mem






