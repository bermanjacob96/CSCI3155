   "DoCall" should "succeed in base case" in {
     val params = ("x",MTyp(MConst,TNumber)) :: Nil
     val eFuncBody = Binary(Plus,Var("x"),Var("x"))
     val f:Expr = Function(None,params,None,eFuncBody)
     val args:List[Expr] = N(1) :: Nil
     val e:Expr = Call(f,args)

     val ep:Expr = Binary(Plus,N(1),N(1))
     assertResult(ep) {
       step(e)
     }
   }