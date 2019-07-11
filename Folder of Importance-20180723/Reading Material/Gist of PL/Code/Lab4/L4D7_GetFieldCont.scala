 /*
  * @file 		L4D7_GetFieldCont.scala
  * @author 	Spencer D Wilson
  * @date 		7/8/17
  *
  * @modifier	<your_name_here>
  * @date
  * 
  * @brief 
  * 	stubs worth considering while implementing logic over GetField
  *		Expr throughout the Lab 4 interpreter
  *
  * @sources
  * 	https://github.com/csci3155/pppl-lab4
  *
  * @operate
  * 	scala <filepath>/L4D7_GetFieldCont.scala
  * 	
  */ 

def typeof(env: TEnv, e: Expr): Typ = e match {
	
	// TypeGetField
	case GetField(e1,f) => {
		???
	}

	case _ => ??? // The rest of typeof that I am not worried about at the moment
}

def substitute(e: Expr, esub: Expr, x: String): Expr = {
	def subst(e: Expr): Expr = e match {

		// SubstPrint
		case Print(e1) => Print(subst(e1))

		// SubstGetField
		// this is a binary operation '.' on 'e1' and 'f'. 
		// Note that e1 is of type Expr and f is not of type Expr
		case GetField(e1,f) => ??? 

      	case _ => ??? // The rest of subst that I am not worried about at the moment
    }
    subst(e)
}


// Note that my pattern matching makes use of excessive 
// variable declarations after you complete the code... 
// remove unnecessary variables
def step(e:Expr):Expr = e match {

	// SearchGetField, DoGetField, StuckErr
    case GetField(e1,f) => e1 match {

    	// DoGetField, StuckErr
    	case Obj(fields) if ( None == fields.find{ case (fiei@(fi,ei)) => ??? } => {
    		// does fields contain the key f ? if it doesn't then throw a stuck error
    		???
    		// fields.getOrElse(???,???)
    	}

    	// StuckErr
    	case e1 if (???) => ???

    	// SearchGetField
    	case e1 => ???
    }

  	case _ => ??? // The rest of step that I am not worried about at the moment

}

def step(e:Expr):Expr = e match {
    // DoGetField
  case GetField( v1 @ Obj(fields),f) if isValue(v1) => {
      fields.getOrElse(f,throw new StuckError(e))
  }
}