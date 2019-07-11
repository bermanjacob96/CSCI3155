 /*
  * @file 		L4D6_ObjCont.scala
  * @author 	Spencer D Wilson
  * @date 		7/8/17
  * 
  * @brief 
  * 	stubs worth considering while implementing logic over Obj
  *		Expr throughout the Lab 4 interpreter
  *
  * @sources
  * 	https://github.com/csci3155/pppl-lab4
  *
  * @operate
  * 	scala <filepath>/L4D6_ObjCont.scala
  * 	
  */ 

def typeof(env: TEnv, e: Expr): Typ = e match {
	case Obj(fields) => {
		// transform Map[String,Expr] of length n
		// to Map[String,Typ] of length n
		val tfields = fields.mapValues{ (ei) => ??? /*should be type Typ, note that ei has type Expr*/}
		// do a little more work to return a Typ
		???
	}
	case _ => ??? // The rest of typeof that I am not worried about at the moment
}

def substitute(e: Expr, esub: Expr, x: String): Expr = {
	def subst(e: Expr): Expr = e match {

		// SubstPrint
		case Print(e1) => Print(subst(e1))

		// SubstObj
		case Obj(fields) => {
			// recurse subst on all things of type Expr in fields
			// transform Map[String,Expr] of length n
			// to Map[String,Expr] of length n where the values might be changed
			val fieldsSub = fields.mapValues{ (ei) => ??? }
			// val fieldsSub = fields.mapValues( ??? ) // also valid... consider the nature of this input to mapValues
			// return the correct type
			???
		}

      	case _ => ??? // The rest of subst that I am not worried about at the moment
    }
    subst(e)
}


// Note that my pattern matching makes use of excessive 
// variable declarations after you complete the code... 
// remove unnecessary variables
def step(e:Expr):Expr = e match {

	// SearchObj, StuckErr
    case Obj(fields) => fields.find{ case (fiei@(fi,ei)) => !isValue(ei) } match {
			// StuckErr
		case None => ???
			// SearchObj
		case Some((fi,ei)) => ??? // recall that extending a map with an existing key to a new value will override the value associated with the key
	}

  	case _ => ??? // The rest of step that I am not worried about at the moment

}}