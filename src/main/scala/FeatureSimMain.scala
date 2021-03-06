import agent._
import visualization._
import predation._
import simulation._
import communication._
import norm.NormativeAgent

import scala.util.Random
import metasim._
import coherence._
import coherence.CoMod._
import javax.swing.JFrame

import scala.meta._
import scala.io.Source
import sys.process._

import visualization._

import learning.xcs._
import learning.xcs.XCSAlgorithm.model





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
  println(cm)*/

  var s = new MUXProblem(10000)
  XCSAlgorithm(s)

  println("Final population in sorted order of fitness")
  var rules = model.sort()
  rules foreach (r => print(r))




  var cm = FeatureConstraintModel {
      context("spatial", "Context Information", 1.0)
      feature("base", "Base Model")
      feature("agentModel", "Prey-Predator")
      feature("preyModel", "Prey Protocol")
      feature("predModel", "Predator Protocol")
      feature("patchModel", "Environment")
      feature("patchWithGrass","Environment with Grass")
      feature("patchWithNoGrass", "Environment without Grass")
    } subjectTo (
      facilitate("agentModel","base",1.0),
      facilitate("patchModel","base", 1.0),
      facilitate( List("preyModel","predModel"), "agentModel",1.0),
      facilitate("patchWithGrass","spatial", 1.0),
      facilitate("patchWithGrass","patchModel"),
      facilitate("patchWithNoGrass","patchModel"),
      conflict("patchWithGrass", "patchWithNoGrass"),
      conflict("patchWithNoGrass", "spatial")
    ) evaluate()



  var rm = cm.generateResolution()
  println("Activated features and dependencies")
  for (r <- rm keys) {
    print(r.id + "---> ")
    rm(r) foreach {x => print(x.id + " ")}
    println()
  }

  println("Serialized feature list:")
  var fs = cm.serialize(rm)

  for (f <- fs) {
    print(f + "  ")
  }
  println()



/*
  var fg = new FeatureGraphVis(cm)
  fg.setDefaultCloseOperation(3)
  fg.setSize(800,800)
  fg.setVisible(true)
*/
/*
  var cmd : String = "ls -al"
  var result = (sys.process.Process(cmd) !!)
  println(result)
  println("Sequence command")
  Seq("ls -al","ls -al") foreach { x => {
    result = (sys.process.Process(x) !!)
    println(result)
  }
  }

*/
/*
  var transformedModel = VariabilityModel
  {
    FeatureSpecifications("/Users/yilmaz/IdeaProjects/FeatureSim/" +
      "src/main/scala/metasim/SourceInput.txt")
  }
  {
    FeatureTree(Base("base"), List(
      And("agentModel", List(
        Feature("preyModel"),
        Feature("predModel")
      )),
      Xor("patchModel", List(
        Feature("patchWithGrass"),
        Feature("patchWithNoGrass"))
      )))
  }
  {
      ResolutionModel(scala.collection.mutable.Map[String, Boolean](
      "patchWithGrass" -> true,
      "patchWithNoGrass" -> false))
  }
*/

  var vmod = VariabilityModel
  {
    FeatureSpecifications {
      "/Users/yilmaz/IdeaProjects/FeatureSim/src/main/scala/SourceInput.txt"
    }
  }
  {
    FeatureConstraintModel {
      context("spatial", "Context Information", 1.0)
      feature("base", "Base Model")
      feature("agentModel", "Prey-Predator")
      feature("preyModel", "Prey Protocol")
      feature("predModel", "Predator Protocol")
      feature("patchModel", "Environment")
      feature("patchWithGrass","Environment with Grass")
      feature("patchWithNoGrass", "Environment without Grass")
    } subjectTo (
      facilitate("agentModel","base",1.0),
      facilitate("patchModel","base", 1.0),
      facilitate( List("preyModel","predModel"), "agentModel",1.0),
      facilitate("patchWithGrass","spatial", 1.0),
      facilitate("patchWithGrass","patchModel"),
      facilitate("patchWithNoGrass","patchModel"),
      conflict("patchWithGrass", "patchWithNoGrass"),
      conflict("patchWithNoGrass", "spatial")
    )
  } //compileCoherenceModel "out.txt"


  println("----------------------------------------")

  vmod.compileFeatures() foreach {f => {
    println("Feature Name: " + f._1)
    println("Feature Source: " + f._2)
    println("--------------------------------------")
  }}


  vmod.addFeature(FeatureGenerator("XXX")(
      featureSpec = source"""
         object PM {
           feature("XXX") {
              trait MyPrey {
                def move(): Unit
                def reproduce(): Unit
                def death(): Unit
                def consume(e: Double): Unit
            }
        }
       }
           """
    ))

    println("----------------------------------------")
    println("----------------------------------------")

    vmod.featureModules foreach {f => {
      println("Feature Name: " + f._1)
      println("Feature Source: " + f._2)
      println("--------------------------------------")
    }}

    vmod.featureCoherenceGraph.evaluate()
    var L = vmod.featureCoherenceGraph.getActiveNodes(0.1)
    println("Activated nodes are ")
    for (p <- L) println(p.id)





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