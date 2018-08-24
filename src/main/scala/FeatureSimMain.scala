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


  var fspec1= FeatureSpec(
    FeatureTree(Base("base"), List(
      Xor("patchModel", List(
        Feature("patchWithGrass"),
        Feature("patchWithNoGrass"))
      ))),
    ResolutionModel(scala.collection.mutable.Map[String,Boolean](
      "patchWithGrass" -> false,
              "patchWithNoGrass" -> true)))

  val s1 = source"""
                 import featuremodel.Collaboration._
                 object FeatureModel {

                   feature("base") {
                     class MyPatch {
                        val posx : Int = 10
                        val posy : Int = 20
                    }
                   }

                   feature("patchModel")  {

                   }

                   feature("patchWithGrass") {
                    trait MyPatch {
                      def generateCommand(): PatchSetUp = {
                        new PatchSetUp
                      }

                      class PatchSetUp extends Command with nvm.CustomAssembled {
                         override def getSyntax = Syntax.commandSyntax(right = List(NumberType, CommandBlockType | OptionalType))
                         def perform(args: Array[api.Argument], context: api.Context): Unit = {
                            var pw = new PrintWriter(new File("/Users/yilmaz/IdeaProjects/example-scala/test.txt"))
                            pw.println("Patch with grass set up")
                            val world = context.getAgent.world.asInstanceOf[agent.World]
                            val eContext = context.asInstanceOf[nvm.ExtensionContext]
                            val nvmContext = eContext.nvmContext
                            var patchColor: String = null
                            val p: Patch = eContext.getAgent.asInstanceOf[Patch]
                            val r = scala.util.Random
                            var index = world.patchesOwnIndexOf("COUNTDOWN")
                            var grt: Double = args(0).getDoubleValue
                            if (r.nextDouble() <= 0.5) {
                                world.patchChangedColorAt(p.id.asInstanceOf[Int], Color.argbToColor(Color.getRGBByName("green")))
                                patchColor = "green"
                                p.setVariable(index, grt.toLogoObject)
                            }
                            else {
                                world.patchChangedColorAt(p.id.asInstanceOf[Int], Color.argbToColor(Color.getRGBByName("brown")))
                                patchColor = "brown"
                                p.setVariable(index, (r.nextDouble() * grt).toLogoObject)
                             }
                           pw.close()
                      }

                      def assemble(a: nvm.AssemblerAssistant) {
                          a.block()
                          a.done()
                      }
                    }
                   }
                  }

                   feature("patchWithNoGrass") {
                      trait MyPatch {
                        def generateCommand(): PatchSetUp = {
                          new PatchSetUp
                        }

                        class PatchSetUp extends Command with nvm.CustomAssembled {
                          override def getSyntax = Syntax.commandSyntax(right = List(NumberType, CommandBlockType | OptionalType))
                          def perform(args: Array[api.Argument], context: api.Context): Unit = {
                            var pw = new PrintWriter(new File("/Users/yilmaz/IdeaProjects/example-scala/test.txt"))
                            pw.println("Patch with grass no grass set up")
                            val world = context.getAgent.world.asInstanceOf[agent.World]
                            val eContext = context.asInstanceOf[nvm.ExtensionContext]
                            val nvmContext = eContext.nvmContext
                            val p: Patch = eContext.getAgent.asInstanceOf[Patch]
                            world.patchChangedColorAt(p.id.asInstanceOf[Int], Color.argbToColor(Color.getRGBByName("green")))
                          }

                          def assemble(a: nvm.AssemblerAssistant) {
                             a.block()
                             a.done()
                          }
                        }
                      }
                  }
        }"""


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