class L4D8Test(lab4: Lab4Like) extends FlatSpec {
  import lab4._
  import jsy.lab4.Parser.parse

  // start with something really basic
  "val" should "find type" in {
    val e:Expr = N(2)
    val t:Typ = TNumber
    assertResult( t ){ typeof(empty, e) }
  }

  "minus" should "find type" in {
    val e:Expr = Binary(Minus,N(2),N(1))
    val t:Typ = TNumber
    assertResult( t ){ typeof(empty, e) }
  }
  it should "step once" in {
    val e:Expr = Binary(Minus,N(2),N(1))
    val ep:Expr = N(1)
    assertResult( ep ){ step(e) }
  }

  // name x = 2 - 1; x
  "NameDecl" should "find type" in {
    val e:Expr = Decl(MName, "x", Binary(Minus, N(2), N(1)), Var("x"))
    val t:Typ = TNumber
    assertResult( t ){ typeof(empty, e) }
  }
  it should "redex" in  {
    val m = MName
    val e:Expr = Binary(Minus,N(2),N(1))
    val b:Boolean = false
    assertResult( b ) { isRedex(m,e) }
  }
  it should "substitute" in {
    val e:Expr = Var("x")
    val esub:Expr = Binary(Minus,N(2),N(1))
    val x:String = "x"
    val eFound:Expr = Binary(Minus,N(2),N(1))
    assertResult( eFound ){ substitute(e, esub, x) }
  }
  it should "one step" in {
    val e:Expr = Decl(MName, "x", Binary(Minus, N(2), N(1)), Var("x"))
    val ep:Expr = Binary(Minus,N(2),N(1))
    assertResult( ep ){ step(e) }
  }
  it should "second step" in {
    val e:Expr = Binary(Minus,N(2),N(1))
    val ep:Expr = N(1)
    assertResult( ep ){ step(e) }
  }

  "FunctionDef" should "type " in {
//    val params:List[(String, MTyp)] = ("x",MTyp(MConst,TNumber)) :: ("y", MTyp(MConst,TNumber)) :: Nil
    val params:List[(String, MTyp)] = List( ("x",MTyp(MConst,TNumber)), ("y", MTyp(MConst,TNumber)) )
    val e:Expr = Function(None, params, None, Binary(Minus, Var("x"), Var("y")))
    val t:Typ = TFunction(params, TNumber)
    assertResult( t ){ typeof(empty, e)
  }


    "Call" should "type" in {
      //    val params:List[(String, MTyp)] = ("x",MTyp(MConst,TNumber)) :: ("y", MTyp(MConst,TNumber)) :: Nil
      val params:List[(String, MTyp)] = List( ("x",MTyp(MConst,TNumber)), ("y", MTyp(MConst,TNumber)) )
      val func:Expr = Function(None, params, None, Binary(Minus, Var("x"), Var("y")))
      val args:List[Expr] = List( N(4), N(1) )
      val e:Expr = Call(func, args)
      val t:Typ = TNumber
      assertResult( t ){ typeof(empty, e) }
    }
    it should "type fail" in {
      //    val params:List[(String, MTyp)] = ("x",MTyp(MConst,TNumber)) :: ("y", MTyp(MConst,TNumber)) :: Nil
      val params:List[(String, MTyp)] = List( ("x",MTyp(MConst,TNumber)), ("y", MTyp(MConst,TNumber)) )
      val func:Expr = Function(None, params, None, Binary(Minus, Var("x"), Var("y")))
      val args:List[Expr] = List( N(4), S("hello") )
      val e:Expr = Call(func, args)
      assertThrows[Throwable] { typeof(empty, e) }
    }
  }
  // then
  // (x:const Number, y:name Number) => { return x - y } (1 - 2, 3 - 4)
}

class L4D8TestRunner extends L4D8Test(jsy.student.Lab4)


class Lab4Suite extends Suites(
  new Lab4SpecRunner,
  new Lab4SpecTypeOfRunner,
  new Lab4SpecStepRunner,
  new L4D8TestRunner,
  new 