package metasim

import scala.meta._

abstract class Graph {
  val Super: Graph
}

class BaseGraph extends Graph {
  var x : Int = 0
  val Super: Graph = null
  def G() {}
}

class CompositeGraph extends Graph {
  var y : Int = 5
  val Super = new BaseGraph {}
  def CG()= {Super.G()}
}

class Composite2Graph extends Graph {
  var y : Int = 5
  val Super = new CompositeGraph {}
  def C2G() = {Super.CG()}
}

abstract class FeatureExpression {var fname: String}
case class And(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Or(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Base(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Feature(name: String) extends FeatureExpression {var fname = name}
case class Optional(name: String, children: List[FeatureExpression]=null) extends FeatureExpression {var  fname = name}
case class Xor(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class FeatureTree(node: FeatureExpression)

object FeatureSpec {
  def apply(): Unit = {
    val n = FeatureTree(Base("F",List(
         And("F0",List(Feature("F01"),Feature("F02"))),
         Or("F1",List(Feature("F11"),Feature("F12"))),
         Optional("F2", List(Feature("F2"))),
         Feature("F3"),
         Xor("F4",List(Feature("F41"),Feature("F42")))
    )))
    var rmodel = scala.collection.mutable.Map[String,Boolean](
      "F11"->true,
      "F12"->false,
      "F2"-> false,
      "F42"-> true
    )

    val m = FeatureTree(Base("base", List(
      Feature("featureb"),
      Feature("featurec")
    )))

    var rmodel1 = scala.collection.mutable.Map[String,Boolean](
      "featureb" -> true,
      "featureb" -> true
    )


    val c  = evaluate(m,rmodel1)
    println(c)
  }


  def evaluate(x: FeatureTree, resMod: scala.collection.mutable.Map[String, Boolean]): String = {
    var composite : String = ""
    x.node match {
      case f : Feature => {composite = composite + "_" + f.name; composite}
      case b : Base => {
        composite = composite + b.name
        b.children foreach { c =>
          composite = composite + evaluate(FeatureTree(c),resMod)
        }
        composite
      }
      case op : Optional => {
        if ((resMod.keys.exists(x => (x == op.fname) && (resMod(op.fname) == true)))) {
          op.children foreach { c =>
            composite = composite + "_" + evaluate(FeatureTree(c),resMod)
          }
        }
        composite
      }
      case a : And => {
        composite = composite + "_" + a.name
        a.children foreach { c =>
            composite = composite + "_" + evaluate(FeatureTree(c),resMod)
        }
        composite
      }
      case o : Or => {
        composite = composite + "_" + o.name
        o.children foreach { c =>
          {
            if ((resMod.keys.exists(x => (x == c.fname) && (resMod(x) == true)))) {
              composite = composite + "_" + evaluate(FeatureTree(c),resMod)
            }
          }
        }
        composite
      }
      case x : Xor => {
        var found = false
        composite = composite + "_" + x.name
        x.children foreach { c =>
          if (found==false) {
            if ((resMod.keys.exists(x => (x == c.fname) && (resMod(x) == true)))) {
              found = true
              composite = composite + "_" + evaluate(FeatureTree(c),resMod)
            }
          }
        }
        composite
      }
    }
  }
}


object MetaTest {

  var classDefined : scala.collection.mutable.Map[Defn.Class,Boolean] =
    scala.collection.mutable.Map()
  var featureMapper = scala.collection.mutable.Map[String,List[Defn.Class]]()
  val pattern = "([A-Za-z0-9]*)_([A-Za-z0-9]+)".r

  def lift(lifter: Defn.Object, base: Defn.Object): Defn.Object = {
    def liftClass(s : Defn.Class, t: Defn.Class): Defn.Class = {
      var composite: Defn.Class = null
      var valList = (s.templ.stats collect { case v: Defn.Val => v.pats }).flatten
      var varList = (s.templ.stats collect { case v: Defn.Var => v.pats }).flatten
      var metList = s.templ.stats collect { case v: Defn.Def => v.name }
      var constructed = List[Stat]()
      t.templ.stats foreach { x => {
        x match {
          case m: Defn.Val => {
            m.pats foreach { n =>
              if (!(valList exists (p => p.toString() == n.toString()))) {
                var pat = p"$n"
                var valType= m.decltpe getOrElse(null)
                var newStmt: Defn.Val = null
                var expr=m.rhs
                if (valType != null)
                   newStmt = q"val $pat : $valType=$expr"
                else
                  newStmt = q"val $pat = $expr"
                constructed = newStmt :: constructed
              }
            }}
            case m: Defn.Var => {
              m.pats foreach { n =>
                if (!(varList exists (p => p.toString() == n.toString())) &&
                     (n.toString!="Super")) {
                  var pat = p"$n"
                  var varType= m.decltpe getOrElse(null)
                  var newStmt: Defn.Var = null
                  var expr=m.rhs getOrElse(null)
                  if (varType != null)
                    newStmt = q"var $pat : $varType=$expr"
                  else
                    newStmt = q"var $pat = $expr"
                  constructed = newStmt :: constructed
                }
              }
            }
            case m: Defn.Def => {
              if (!(metList exists (p => p.toString() == m.name.toString())))
                constructed = m :: constructed
            }
            case _ =>
          }
        }
      }
        constructed = s.templ.stats ::: constructed
        val pattern(prefix,clName) = s.name.toString()
        var cName = Type.Name(lifter.name.toString()+base.name.toString()+"_"+clName)
        var basecName = Type.Name("Base_" + clName)
        var baseType = Init(basecName,Name(""),Nil)
        val decorated = Init(Type.Name(base.name.toString()+"_"+clName),Name(""),Nil)

        q"""
           class $cName extends $baseType {
              var Super = new $decorated {}
                 ..$constructed
            }
        """

      }
    var compositeStmts : List[Stat] = null
    var lifterCls = lifter collect {case cl: Defn.Class => cl}
    var baseCls = base collect {case cl: Defn.Class => cl}
    var baseMinusLifter = List[Defn.Class]()
    var lifterMinusBase = List[Defn.Class]()
    var joint = scala.collection.mutable.Map[String,(Defn.Class,Defn.Class)]()
    var baseInJoint = List[String]()
    lifterCls foreach {cl => {
        val pattern(prefix,clName) = cl.name.toString()
        var found = false
        var c = baseCls find (x => {
          val pattern(pre,xname) = x.name.toString()
          xname==clName
        })
        var foundClass : Defn.Class =null
        c match {
          case Some(c) => {found=true; foundClass=c}
          case None => found = false
        }
        if (found) {
          joint = joint + (clName -> (cl,foundClass))
          val pattern(pre, cname) = foundClass.name.toString()
          baseInJoint = cname:: baseInJoint
        }
        else lifterMinusBase = cl :: lifterMinusBase
      }
      var refinedCls = List[Defn.Class]()

      joint foreach { m =>
        {
          var lfter = m._2._1
          var base = m._2._2
          refinedCls = liftClass(lfter,base) :: refinedCls
        }
      }

      compositeStmts  = (refinedCls ::: lifterMinusBase) ::: baseCls

    }

    var featureName : Term.Name = null

    if (base.name.toString() == "AbstractBase")
      featureName = Term.Name(lifter.name.toString())
    else
      featureName = Term.Name(lifter.name.toString()+base.name.toString())

    q"""
       object $featureName {
              ..$compositeStmts
         }
     """
  }

  def initialize(objs: List[Defn.Object]): Unit = {
    def initializeFeature(classes: List[Defn.Class], featureName: Term.Name): Unit = {
      classes foreach { cl =>
      {
        if (!(classDefined.keys exists(c=>c.name.toString() == cl.name.toString()))) {
          classDefined += (cl -> true)
          var cName = Type.Name(featureName+"_"+cl.name.toString)
          var basecName = Type.Name("Base_" + "Abstract" + cl.name.toString())
          var baseType = Init(basecName,Name(""),Nil)
          var cStats = cl.templ.stats
          featureMapper("AbstractBase") =
            q"""abstract class $basecName {
                var Super : $basecName =null
            }
             """ :: featureMapper("AbstractBase")
          featureMapper(featureName.toString()) =
            q"""class $cName extends $baseType {
                 ..$cStats
                }
                """ :: featureMapper(featureName.toString())
        }
        else {
          var cName = Type.Name(featureName+"_"+cl.name.toString)
          var basecName = Type.Name("Base_" + "Abstract" + cl.name.toString())
          var baseType = Init(basecName,Name(""),Nil)
          var cStats = cl.templ.stats
          featureMapper(featureName.toString()) =
            q"""class $cName extends $baseType {
                 ..$cStats
                }
                """ :: featureMapper(featureName.toString())
        }
      }
      }
    }

    featureMapper += ("AbstractBase" -> List[Defn.Class]())
    objs foreach {o => {
        featureMapper += (o.name.toString() -> List[Defn.Class]())
        var cls = o collect {case cl: Defn.Class => cl}
        initializeFeature(cls,o.name)
      }
    }
  }




  def apply(): Unit = {

    val f =  source"""  import Collaboration._
  object base {
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

  object featureb {
    refines {
      class Graph {
        def newGraphMethod() : Unit = {

        }
      }
    }

    refines {
      class Edge(n: featurea.Node) {
        def newEdgeMethod() : Unit = {

        }
      }
    }

    refines {
      class Node
    }

    class Weight {
        var w : Double = 0
    }

}
  object featurec {
    refines {
      class Node {
        def newNodeMethod() : Unit = {

        }
      }
    }
  }
      """


    var clss = f.collect { case cls: Defn.Object => cls }
    initialize(clss)
    //var cf = featureMerge1(clss(0),clss(1))
    featureMapper foreach { f=>
      {
        println("feature name: " + f._1)
        println("--------------")
        println(f._2)

        f._2 foreach {cl => {
          println("Analyzing " + cl.name.toString())
          println("-------------")
          cl.templ.stats foreach { x => {
            x match {
              case m: Defn.Val => {println("val defn " + m.decltpe + " " + m.pats)}
              case m: Defn.Var => {println("var defn " + m.decltpe + " " + m.pats)}
              case m: Defn.Def => {println("def defn " + m.decltpe + " " + m.name)}
              case _ =>
            }
          }}
        }}
      }

    }
    println("Feature merge test")
    println("==================")
    var lftStmts : List[Stat] = featureMapper("base")
    println("base statements")
    println("===============")
    println(lftStmts)
    var baseStmts : List[Stat] = featureMapper("AbstractBase")
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

    println("===========")
    println("Merge base with featureb")
    println("===========")
    var fbStmts : List[Stat] = featureMapper("featureb")
    var cmp2 = lift(q"""
         object featureb {
              ..$fbStmts
         }
       """,cmp)

    println(cmp2)

    println("*****************")



  }

  def testMeta(): Unit = {
    val f =  source"""  import Collaboration._
  object base {
   abstract class Graph {
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
   abstract class Node {
     def test2() : Int = {
      var y = 10
     }
   }

   abstract class Edge {
      var e : Int = 15
   }

 }

  object featureb {
    refines {
      class Graph {
        def newGraphMethod() : Unit = {

        }
      }
    }

    refines {
      class Edge(n: featurea.Node) {
        def newEdgeMethod() : Unit = {

        }
      }
    }

    refines {
      class Node
    }

    class Weight {

    }

}"""


    var clss = f.collect { case cls: Defn.Object  => cls }
    var progStr : Tree = null

    clss foreach { x =>
      println(x.structure)
      println(x.name)
      x.templ.stats foreach { y =>
      {
        if (y.children.length > 3) {
          y.children(3).children foreach { z =>
          {
            if (z.isInstanceOf[Defn.Def])  {
              println("function name is " + z.asInstanceOf[Defn.Def].name)
              var method = z.transform {
                case q"def test1() : Int = {$body}" =>
                  q"def test3() : Int = {$body}"
                case q"def test2() : Int = {$body}" =>
                  q"def test4() : Int = {$body}"
              }
              println("new method is " + method)

            }
          }
          }
        }
      }

      }

    }


    val y = "val x = 2".tokenize.get
    println(y)


    val myf = q"def myF() : Unit = {var lv : Int = 0}"


    myf match {
      case q"def $name(...$paramss): $result = { ..$body }" =>
        println(q"def $name: Unit = { ..$body }")
    }

    val m = q"case class User(name: String, age: Int)"
    println(m.name)

    println("object Main extends App { println(1) }".parse[Source].get)

    val c = source"""trait Op[A]
    object Op extends B {
      case class Foo(i: Int) extends Op[Int] {var x : Int = 0}
      case class Bar(s: String) extends Op[String]
    }""".collect { case cls: Defn.Class => cls }

    println(c)

    println(c.head)

    println(c.head.tparams)

    val x1 = q"myList.filter(_ > 3 + a).headOption; myList1.filter(_ > 3 + a).headOption"

    println(x1)


    val x2 = x1.transform {
      case q"$lst.filter($cond).headOption" => q"$lst.find($cond)"
    }

    println(x2)

    val x3 = q"val x = 2;  class Z { def m1 : Unit = {}}"
    println(x3)


    val x4 = x3.transform {
      case q"class $lst {$body}" =>
        q"class $lst {$body; def m3 : Unit = {}} "
    }

    println(x4)
  }
}
