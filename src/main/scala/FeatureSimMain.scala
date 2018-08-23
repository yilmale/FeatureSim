import agent._
import featuremodel._
import visualization._
import predation._
import simulation._
import communication._
import norm.NormativeAgent

import scala.util.Random
import metasim._
import coherence._
import coherence.CoMod._
import scala.meta._


object FeatureSimMain extends App {



 /*
  val cm = CoherenceModel {
        data(("B0", "evidence1", 1.0))
        belief("B1", "belief1")
        belief("B2", "belief2")
        belief("B3", "belief3")
        goal("G1", "goal1")
        goal("G2", "goal2")
  } subjectTo (
        explain( List("B1","B2"), "B0",0.2),
        contradict("B2", "B3")
    ) subjectTo (
        deduce("B3", "G1"),
        deduce("B2", "G2"),
        incompatible("G1", "G2")
    )

  cm.evaluate()
  println(cm)


*/
  import FeatureComposer._

/*  var fspec= FeatureSpec(
    FeatureTree(Base("base"), List(
      Feature("featureb"),
      Feature("featurec"))),
    ResolutionModel(scala.collection.mutable.Map[String,Boolean](
      "featureb" -> true,
      "featurec" -> true)))

  val s = source"""
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

      trait Weight {
        def newWeightMethod() : Unit = {}
      }
    }
  }"""
*/
  var fspec1= FeatureSpec(
    FeatureTree(Base("base"), List(
      Feature("patchWithGrass"))),
    ResolutionModel(scala.collection.mutable.Map[String,Boolean](
      "patchWithGrass" -> true)))

val s1 = source"""
                  import featuremodel.Collaboration._
                 object FeatureModel1 {
                   feature("base") {
                   class MyPatch {
                      class PatchSetUp {
                        def apply(): Unit = {
                          println("A-A1")
                        }
}
                    }
                  }

              feature("patchWithGrass") {
                    trait MyPatch {
                      class A1 {
                        def apply(): Unit = {
                          println("PatchWithGrass-A1")
                        }
                      }
                    }
                  }
                 }
  """


  FeatureComposer(s1)
  var composite = reduce(merge(fspec1))
  println("Composed program")
  println("=================")
  println(composite)


  //var i = new InferenceTest()

  /*
  new Experiment {
    params = Map("stopTime" -> 20)
    new Context {
      var s = (new Space2D(750,750)) createSpace2D()
      setProjection(s)
      model = new Model {
        object ag extends AgentGenerator
        val preyGen = FOModel(PreyModel)
        for (j <- 0 until 10) {
          var na = NormativeAgent()
          add(na)
        }
      }
    } simulate()
  }

  */
  /*
    val m : scala.xml.Node = Communication.testComm()
    println(m)
    new Experiment with PredationAnalysis  {self  =>
      expType = "Factorial"
      params = Map("stopTime" -> 20, "xdim" -> 10, "ydim" -> 10, "predDeathProb" -> 0.1, "predReproduce" -> 0.5,
        "reproductionRate" -> 0.1, "foliageRate" -> 0.5, "name" -> "test")
      new Context with GridView {
        val g = createGridView(param("xdim").asInstanceOf[Int], param("ydim").asInstanceOf[Int])
       // createGridVis(g)
        setProjection(g)
        model = new Model {
          object ag extends AgentGenerator
          val preyGen = FOModel(PreyModel)
          for (i <- 0 to 20) {
            val prey = ag.Cons(PreyModel.stateModel)
            preyGen.Cons(prey)
          }
          for (j <- 0 to 20) add(new Predator())
        }
        monitor = new PredationMonitor()
      } simulate()
    } analyze()
  */


}