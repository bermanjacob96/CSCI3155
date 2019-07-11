def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)
    def ty(e:Expr):Typ = typeof(env,e)

    e match {

      case Obj(keyVals) => TObj(keyVals map { kv => { val k = kv._1; ??? } })
      case Obj(keyVals) => TObj(keyVals map { case ((k,v)) =>  ??? })

      case Obj(keyVals) => TObj(keyVals mapValues ty)
      case Obj(keyVals) => TObj(keyVals.mapValues(ty))


      case Obj(keyVals) => {
        val keyTypes = keyVals mapValues ty
        TObj(keyTypes)
      }
      case Obj(keyVals) => {
        val keyTypes = keyVals.mapValues(ty)
        TObj(keyTypes)
      }

      case Obj(keyVals) => {
        val keyTypes = keyVals mapValues { (ei) => typeof(env,ei) }
        TObj(keyTypes)
      }
      case Obj(keyVals) => {
        val keyTypes = keyVals.mapValues({ (ei) => typeof(env,ei) })
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
      case Obj(keyVals:Map[String,Expr]) => {
        val keyTypes:Map[String,Typ] = {
          keyVals.mapValues({
            (ei:Expr) => {
              val ti:Typ = typeof(env,ei)
              ti
            }
          }
        })
        TObj(keyTypes)
      }


      case Obj(fields:Map[String,Expr]) => {
        val tFields:Map[String,Typ] = ???
        TObj(tFields)
      }
    }
  }