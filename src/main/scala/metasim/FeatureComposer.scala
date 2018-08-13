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
      var traitType = Init(trname,Name(""),Nil)

      q"""class $clname extends Base_FeatureModel with $traitType {
              ..$cstmts
         }"""
    }

    var baseCls = base collect {case c : Defn.Class => c}
    var lftTrs = lifter collect {case t : Defn.Trait => t}
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

    var lftCls = lifter collect {case c : Defn.Class => c}
    refinedCls = refinedCls ::: lftCls

    var baseTrs = base collect {case t : Defn.Trait => t}

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
            //var cName = Type.Name(featureName+"_"+cl.name.toString)
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
      var cls : List[Defn] = o collect {
        case cl: Defn.Class => cl
        case tr : Defn.Trait => tr}
      initializeFeature(cls,o.name)
    }}
  }

  def apply(): Unit = {

    val f = source"""
    import Collaboration._
    object FeatureModel {

     feature("base") {
    class Graph {
     var a1 : Int = 0
     var a2 : Int = 1

     def myPrint() : Int = {
      var x = 5
      x
    }

    def test1() : Int = {
       var y = 10
    }
   }
   class Node {
     def test2() : Int = {
      var y = 10
     }
   }

   class Edge {
      var e : Int = 15
   }

 }

  feature("featureb") {
      trait Graph {
        def newGraphMethod() : Unit = {
        }

    }

    trait Edge {
        def newEdgeMethod() : Unit = {

        }
    }

    trait Node { }

    class Weight {
        var w : Double = 0
    }
    }

  feature("featurec") {
    trait Node {
        def newNodeMethod() : Unit = {

        }
      }
  }
  }"""


    var listObjs = List[Defn.Object]()

    var fns = f collect {case o: Defn.Object  => o}
    fns(0).templ.stats foreach { s =>
      {
        var stmts = List[Stat]()
        s match {
        case m : Term.Apply => {
          val pattern1(_,x,_) = m.fun.toString()
          println("function application: " + x + " " + m.args)
          m.args(0).children collect {
            case c : Defn.Class => stmts = c :: stmts
            case tr : Defn.Trait => stmts = tr  :: stmts
          }

          var t = Term.Name(x)
          listObjs = q"object $t { ..$stmts}" :: listObjs
        }
        case _ => //println("something else")
      }
      }

    }

    println("Transformed.....")
    println(listObjs)

    //var clss = f.collect { case cls: Defn.Object => cls }
    initialize(listObjs)
    //initialize(clss)
    featureMapper foreach { f=>
    {
      println("feature name: " + f._1)
      println("--------------")
      println(f._2)
    }
    }

    var lftStmts : List[Stat] = featureMapper("base")
    println("base statements")
    println("===============")
    println(lftStmts)
    var baseStmts : List[Stat] = featureMapper("FeatureModel")
    println("Abstract Base Statements")
    println("========================")
    println(baseStmts)
    println("After merge")
    println("===========")
    var cmp = lift(
      q"""
         object base {
              ..$lftStmts
         }
       """,
      q"""object AbstractBase {
              ..$baseStmts
         } """)

    println(cmp)
    println("After merge")
    println("===========")
    var fbStmts : List[Stat] = featureMapper("featureb")
    var cmp2 = lift(
      q"""
         object featureb {
              ..$fbStmts
         }""",cmp)
    println(cmp2)

  }




}