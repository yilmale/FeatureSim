package metasim

import scala.meta._

abstract class Graph {
  var Super: Graph
}

class BaseGraph extends Graph {
  var x : Int = 0
  var Super: Graph = null
}

class CompositeGraph(g: Graph) extends Graph {
  var y : Int = 5
  var Super = g

  def lift(g: Graph): Unit = {
    Super = g
  }
}

abstract class FeatureExpression
case class Node(name: String, nodeType: String) extends FeatureExpression
case class And(name:String, children: List[FeatureExpression]) extends FeatureExpression
case class Or(name:String, children: List[FeatureExpression]) extends FeatureExpression
case class Feature(name: String) extends FeatureExpression
case class FeatureTree(node: FeatureExpression) extends FeatureExpression

object FeatureSpec {
  def apply(): Unit = {
    val n = FeatureTree(And("F",List(Feature("F1"),Feature("F2"))))
    evaluate(n)
  }

  def evaluate(x: FeatureTree): Unit = {
   //x.node match {
   //   case
    //}
  }
}


object MetaTest {

  var classDefined : scala.collection.mutable.Map[Defn.Class,Boolean] =
    scala.collection.mutable.Map()
  var classList : Set[Defn.Class] = Set()
  val pattern = "([A-Za-z0-9]*)_([A-Za-z0-9]+)".r

  def initialize(objs: List[Defn.Object]): Unit = {
    objs foreach {o => {
        var cls = o collect {case cl: Defn.Class => cl}
        initializeFeature(cls,o.name)
      }
    }
  }

  def initializeFeature(classes: List[Defn.Class], featureName: Term.Name): Unit = {
    classes foreach { cl =>
    {
      if (!(classDefined.keys exists(c=>c.name.toString() == cl.name.toString()))) {
        classDefined += (cl -> true)
        var cName = Type.Name(featureName+"_"+cl.name.toString)
        var basecName = Type.Name("Base_" + cl.name.toString())
        var baseType = Init(basecName,Name(""),Nil)
        var cStats = cl.templ.stats
        classList = (classList +
          q"""abstract class $basecName {
                var Super : $basecName
            }
             """) +
          q"""class $cName extends $baseType {
                 var Super = null
                 ..$cStats
                }
                """
      }
      else {
        var cName = Type.Name(featureName+"_"+cl.name.toString)
        var basecName = Type.Name("Base_" + cl.name.toString())
        var baseType = Init(basecName,Name(""),Nil)
        var cStats = cl.templ.stats
        classList = classList  +
          q"""class $cName extends $baseType {
                 var Super = null
                 ..$cStats
                }
                """
      }
    }
    }
  }

  def lift(s : Defn.Class, t: Defn.Class): Unit = {

  }

  def featureMerge(base: Defn.Object, lifter: Defn.Object): Defn.Object = {
    var fts0 = base.collect {case cls: Defn.Class => cls}
    var fts1 = lifter.collect {case cls: Defn.Class => cls}


    val pattern(prefix,className) = "Feature1Feature2_Graph"

    println(prefix)
    println(className)

    fts1 foreach { cl => {
      val pattern(prefix,clName) = cl.name.toString()
      fts0 foreach {liftCl => {
          val pattern(liftprefix,liftName) = liftCl.name.toString()
          if (clName == liftName) {
            lift(liftCl,cl)
          }
         }
       }
      }
     }



    var stmts : List[Defn.Class] = classList.toList
    var composite : Defn.Object = q"object CM {..$stmts}"
    composite

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

}"""


    var clss = f.collect { case cls: Defn.Object if cls.name.toString=="base" => cls }
    initialize(clss)
    //var cf = featureMerge1(clss(0),clss(1))
    println(classList)


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
