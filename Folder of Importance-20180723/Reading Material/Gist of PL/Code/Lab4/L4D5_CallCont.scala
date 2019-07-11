 /*
  * @file 		L4D5_CallCont.scala
  * @author 	Spencer D Wilson
  * @date 		7/8/17
  * 
  * @brief 
  * 		Here is an additional stub for call in step that I encourage you to play around with
  *			You are welcome to use this as part of your final solution
  *			
  * 		Note that my pattern matching makes use of excessive variable declarations
  *			after you complete the code... remove unnecessary variables
  *
  * @sources
  * 	https://github.com/csci3155/pppl-lab4
  *
  * @operate
  * 	scala <filepath>/L4D5_CallCont.scala
  * 	
  */ 

def step(e:Expr):Expr = e match {

	// SearchCall2, DoCall, DoCallRec, StuckError
	case Call(v1@Function(p,params,tann,ep),args) => {

		// useful list zip or parameters with arguments
		val pazip = params zip args

		// codition to discover if I am in search or do... something about reducability
		val b = pazip.exists { case (p@(x,mt@MTyp(m,t)),arg) => ??? }
		// with optimal variables... val b = pazip.exists { case ((_,MTyp(m,_)),e) => ??? }

		// DoCall, DoCallRec, StuckError
		if (!b) {
			// many routes are possible...
			// here is the one that makes the most sense to me...
			// this has some redundancy in it's coding
			p match{
				  // DoCall
				case None => pazip.foldRight(???){ case ((p@(x,mt@MTyp(m,t)),arg),acc) => ??? } // need to perform substitution
				  // DoCallRec
				case Some(x) => {
				  val argsSubIntoFBody = pazip.foldRight(???){ case ((p@(x,mt@MTyp(m,t)),arg),acc) => ??? } // same as above...
				  ??? // still need to do a little more substitution
				}
			}
		}
		// SearchCall2
		else {
		  val pazipUpdated = mapFirst(pazip){ case (p@(x,mt@MTyp(m,t)),arg) => ??? } // must return Option[((String,MTyp),Expr)]
		  // I need to get out my arguments from pazipUpdated...
		  val argsUpdated = pazipUpdated.foldRight(Nil:List[Expr]){ case ( (p@(x,mt@MTyp(m,t)),arg) , acc ) => ??? } // unzip might also work...
		  // return a Call(_,_) node... that has the same function from before and an updated instance of the argument list
		  ???
		}
	}

	// StuckErr
	// The type checker should have made certain that 
	// for e1(e2)
	// e1, when steped to a value, has a function type
	// So if it is not... iterateStep should locate a StuckErr
	// This is an indication that something is wrong/incomplete in 
	// our implimentation of the typeof function
	case Call(v1,e2) if isValue(v1) => ??? 

	// SearchCall1
	case Call(e1,e2) => ???

	case _ => ??? // The rest of step that I am not worried about at the moment

}