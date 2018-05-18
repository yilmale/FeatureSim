import agent._
import featuremodel._
import visualization._
import predation._
import simulation._
import communication._
import norm.NormativeAgent

import scala.util.Random
import metasim.MetaTest
import coherence._
import coherence.CoMod._


object FeatureSimMain extends App {


  val cm = CoherenceModel {
    data(("B0", "evidence1", 0.7))
    belief("B1", "belief1",0.5)
    belief("B2", "belief2", 0.6)
    belief("B3", "belief3", 0.2)
  } subjectTo (
    explain( List("B1","B2"), "B0", 0.1),
    explain("B1", "B3", 0.1),
    contradict("B2", "B3", 0.1)
  )

  println(cm)





  //MetaTest()
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