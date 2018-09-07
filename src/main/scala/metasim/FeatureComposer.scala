package metasim

import scala.meta._




object FeatureComposer {

  var featureMapper = scala.collection.mutable.Map[String,List[Defn]]()
  val pattern = "([A-Za-z0-9]*)_([A-Za-z0-9]+)".r
  val pattern1 = "([A-Za-z0-9(]+)\"([A-Za-z0-9]+)\"([)])".r

  def lift(lifter: Defn.Object, base: Defn.Object): Defn.Object = {
    def liftClass(s : Defn.Class, t: Defn.Trait): Defn.Class = {
      var clname = s.name
      var trname = t.name
      var cstmts = s.templ.stats
      var sInits = s.templ.inits
      var traitType = Init(trname,Name(""),Nil)
      sInits = sInits ::: List(traitType)
      if (sInits.head.name.toString() != "Base_FeatureModel")
        sInits = Init(Type.Name("Base_FeatureModel"), Name("Base_FeatureModel"), Nil) :: sInits

      q"""class $clname extends ..$sInits {
              ..$cstmts
         }"""
    }


    //var baseCls = base collect {case c : Defn.Class => c}
    var baseCls = List[Defn.Class]()

    base.templ.stats foreach { c =>
      c  match {
        case c : Defn.Class => baseCls = c :: baseCls
        case _ =>
      }
    }

    var lftTrs = List[Defn.Trait]()
    //var lftTrs = lifter collect {case t : Defn.Trait => t}

    lifter.templ.stats foreach {t =>
      t match {
        case t : Defn.Trait => lftTrs = t :: lftTrs
        case _ =>
      }
    }


    var foundTrait : Defn.Trait = null
    var refinedCls = List[Defn.Class]()

    baseCls foreach { b =>
    {
      var found = false
      val pattern(prefix,bname) = b.name.toString()
      var f = lftTrs find (x => {
        val pattern(tprefix,tname) = x.name.toString()
        tname == bname
      })
      f match {
        case Some(x) => {found = true; foundTrait = x}
        case None =>
      }

      if (found)
        refinedCls = liftClass(b,foundTrait) :: refinedCls
      else
        refinedCls = b :: refinedCls

     }
    }

    //var lftCls = lifter collect {case c : Defn.Class => c}
    var lftCls = List[Defn.Class]()

    lifter.templ.stats foreach { c =>
      c  match {
        case c : Defn.Class => lftCls = c :: lftCls
        case _ =>
      }
    }

    refinedCls = refinedCls ::: lftCls

    //var baseTrs = base collect {case t : Defn.Trait => t}

    var baseTrs = List[Defn.Trait]()

    base.templ.stats foreach {t =>
      t match {
        case t : Defn.Trait => baseTrs = t :: baseTrs
        case _ =>
      }
    }

    var compositeStmts : List[Defn] = (refinedCls ::: baseTrs) ::: lftTrs

    var featureName = Term.Name(lifter.name.toString() + "_" + base.name.toString())


    q"""
       object $featureName {
              ..$compositeStmts
         }
     """
  }

  def initialize(objs: List[Defn.Object]): Unit = {
    def initializeFeature(defns: List[Defn], featureName: Term.Name): Unit = {
      defns foreach { d =>
      {
        d match {
          case c : Defn.Class => {
            var cStats = c.templ.stats
            var cName = Type.Name(featureName.toString()+"_"+c.name.toString)
            featureMapper(featureName.toString()) =
              q"""class $cName {
                 ..$cStats
                }
                """ :: featureMapper(featureName.toString())

          }
          case t : Defn.Trait => {
            var tStats = t.templ.stats
            var tName = Type.Name(featureName + "_"+ t.name.toString)
            featureMapper(featureName.toString()) =
              q"""trait $tName {
                 ..$tStats
                }
                """ :: featureMapper(featureName.toString())
          }
        }
      }
      }

    }

    featureMapper += ("FeatureModel" -> List(q"class Base_FeatureModel {}"))
    objs foreach {o =>
    {
      featureMapper += (o.name.toString() -> List[Defn]())

      var dfns = List[Defn]()
      o.templ.stats foreach {s =>
        s  match {
          case c : Defn.Class => dfns = c :: dfns
          case t : Defn.Trait => dfns = t :: dfns
          case _ =>
        }
      }

      /*var cls : List[Defn] = o collect {
        case cl: Defn.Class => cl
        case tr : Defn.Trait => tr
      }*/


      initializeFeature(dfns,o.name)
    }
    }
  }

  def transform(f: Source): List[Defn.Object] = {
    var listObjs = List[Defn.Object]()

    var fns = f collect {case o: Defn.Object  => o}
    fns(0).templ.stats foreach { s =>
    {
      var stmts = List[Stat]()
      s match {
        case m : Term.Apply => {
          val pattern1(_,x,_) = m.fun.toString()
          m.args(0).children collect {
            case c : Defn.Class => stmts = c :: stmts
            case tr : Defn.Trait => stmts = tr  :: stmts
          }
          var t = Term.Name(x)
          listObjs = q"object $t { ..$stmts}" :: listObjs
        }
        case _ =>
      }
    }

    }
    listObjs
  }

  def reduce(merged: Defn.Object): Defn.Object = {
    var stmts = List[Stat]()
    merged.templ.stats foreach { s =>
      {
        s match {
          case c: Defn.Class => {
            var newName : Type.Name = null
            if (c.name.toString() != "Base_FeatureModel") {
              val pattern(pre, name) = c.name.toString()
              newName = Type.Name(name)
            }
            else newName = c.name
            var clStmts = c.templ.stats
            var cInits = c.templ.inits
            stmts =
              q"""class $newName extends ..$cInits {
                 ..$clStmts
                 } """:: stmts
          }
          case _ => stmts = s :: stmts
        }
      }
    }
    var reduceName = merged.name
    q"object $reduceName {..$stmts}"
  }

  def merge(features: Array[String]): Defn.Object = {
    var lftStmts : List[Stat] = featureMapper(features(0))
    var baseStmts : List[Stat] = featureMapper("FeatureModel")
    var lifter = Term.Name(features(0))
    var composition = lift(
      q"""
         object $lifter {
              ..$lftStmts
         }
       """,
      q"""object AbstractBase {
              ..$baseStmts
         } """)

    for (i <- 1 until features.length) {
       lftStmts = featureMapper(features(i))
       lifter = Term.Name(features(i))
       composition = lift(q"""
         object $lifter {
              ..$lftStmts
         }
       """,composition)
    }
    composition
  }

  def apply(f: Source): Unit = {

    initialize(transform(f))

    featureMapper foreach { f=>
    {
      println("feature name: " + f._1)
      println("--------------")
      println(f._2)
    }
    }


  }




}