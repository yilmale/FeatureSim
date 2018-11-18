/*
package metasim

import org.nlogo.api.ScalaConversions._
import org.nlogo.core.Syntax
import org.nlogo.core.Syntax._
import org.nlogo.{agent, api, core, nvm}
import org.nlogo.api._
import java.io._

import Collaboration._
object FeatureModel {

  feature("base") {
    class MyPatch {

    }

    class MyPrey {

    }

    class MyPred {

    }
  }

  feature("agentModel") {

    trait MyPrey {
      def move()(implicit context: api.Context, r : scala.util.Random, t: agent.Turtle): Unit = {
        val world = context.getAgent.world.asInstanceOf[agent.World]
        t.heading((t.heading + (r.nextDouble()*50)) - (r.nextDouble()*50))
        t.jump(1.0)

        //reduce energy
        var index = world.turtlesOwnIndexOf("ENERGY")
        t.setVariable(index,(t.getVariable(index).asInstanceOf[Double]-1).toLogoObject)
      }

      def reproduce()(implicit context: api.Context, r : scala.util.Random, t: agent.Turtle): Unit = {
        val eContext = context.asInstanceOf[nvm.ExtensionContext]
        val world = context.getAgent.world.asInstanceOf[agent.World]
        var index = world.turtlesOwnIndexOf("ENERGY")
        var e : Double  = t.getVariable(index).asInstanceOf[Double]
        e = e / 2
        t.setVariable(index,e.toLogoObject)
        var c : org.nlogo.agent.Turtle = t.hatch(t.getBreed())
        eContext.workspace.joinForeverButtons(c)
        c.heading(c.heading + (r.nextDouble()*360))
        c.jump(1.0)
      }

      def death()(implicit context: api.Context, t: agent.Turtle): Unit = {
        val eContext = context.asInstanceOf[nvm.ExtensionContext]
        val world = context.getAgent.world.asInstanceOf[agent.World]
        var index = world.turtlesOwnIndexOf("ENERGY")
        if (t.getVariable(index).asInstanceOf[Double] < 0) t.die()
      }

      def consume(e: Double)(implicit context: api.Context, t: agent.Turtle) : Unit  = {
        val world = context.getAgent.world.asInstanceOf[agent.World]
        var index = world.turtlesOwnIndexOf("ENERGY")
        var p = t.getPatchHere
        if (p.pcolor==Color.argbToColor(Color.getRGBByName("green"))) {
          p.setVariable(2, Color.argbToColor(Color.getRGBByName("brown")))
          t.setVariable(index,(t.getVariable(index).asInstanceOf[Double]+e).toLogoObject)
        }
      }
    }


    trait MyPred {
      def move()(implicit context: api.Context, r: scala.util.Random, t: agent.Turtle): Unit = {
        val world = context.getAgent.world.asInstanceOf[agent.World]
        t.heading((t.heading + (r.nextDouble() * 50)) - (r.nextDouble() * 50))
        t.jump(1.0)

        //reduce energy
        var index = world.turtlesOwnIndexOf("ENERGY")
        t.setVariable(index, (t.getVariable(index).asInstanceOf[Double] - 1).toLogoObject)
      }

      def reproduce()(implicit context: api.Context, r: scala.util.Random, t: agent.Turtle): Unit = {
        val eContext = context.asInstanceOf[nvm.ExtensionContext]
        val world = context.getAgent.world.asInstanceOf[agent.World]
        var index = world.turtlesOwnIndexOf("ENERGY")
        var e : Double  = t.getVariable(index).asInstanceOf[Double]
        e = e / 2
        t.setVariable(index,e.toLogoObject)
        var c : org.nlogo.agent.Turtle = t.hatch(t.getBreed())
        eContext.workspace.joinForeverButtons(c)
        c.heading(c.heading + (r.nextDouble()*360))
        c.jump(1.0)
      }

      def death()(implicit context: api.Context, t: agent.Turtle): Unit = {
        val world = context.getAgent.world.asInstanceOf[agent.World]
        var index = world.turtlesOwnIndexOf("ENERGY")
        if (t.getVariable(index).asInstanceOf[Double] < 0) t.die()
      }

      def consume(e: Double)(implicit context: api.Context, t: agent.Turtle): Unit = {
        val world = context.getAgent.world.asInstanceOf[agent.World]
        val eContext = context.asInstanceOf[nvm.ExtensionContext]
        var index = world.turtlesOwnIndexOf("ENERGY")

        var t = eContext.getAgent.asInstanceOf[org.nlogo.agent.Turtle]
        var p = t.getPatchHere

        var trts = p.turtlesHere().iterator()
        var found = false
        while ((trts.hasNext()) && (found == false)) {
          var b = trts.next()
          if (b.getBreed().printName == "SHEEP") {
            found = true
            b.die()
            t.setVariable(index, (t.getVariable(index).asInstanceOf[Double] + e).toLogoObject)
          }
        }
      }

    }


  }

  feature("preyModel") {

    trait MyPrey {
      def move()(implicit context: api.Context, r: scala.util.Random, t: agent.Turtle): Unit
      def reproduce()(implicit context: api.Context, r: scala.util.Random, t: agent.Turtle): Unit
      def death()(implicit context: api.Context, t: agent.Turtle): Unit
      def consume(e: Double)(implicit context: api.Context, t: agent.Turtle): Unit

      def generateCommandPreyAct(): PreyAction = {
        new PreyAction
      }

      def generateCommandPreyInit() : PreyCreator = {
        new PreyCreator
      }

      class PreyCreator extends Command with nvm.CustomAssembled {
        override def getSyntax = commandSyntax(right = List(NumberType, CommandBlockType | OptionalType),
            agentClassString = "O---",
            blockAgentClassString = Some("-T--"))
        private val white = Double.box(9.9)

        def perform(args: Array[api.Argument], context: api.Context): Unit = {
          val n = args(0).getIntValue
          val world = context.getAgent.world.asInstanceOf[agent.World]
          val eContext = context.asInstanceOf[nvm.ExtensionContext]
          val nvmContext = eContext.nvmContext
          val r = scala.util.Random
          var index = world.turtlesOwnIndexOf("ENERGY")

          for(_ <- 0 until n) {
            val turtle = world.createTurtle(world.breeds.get("SHEEP"))
            turtle.size(1.5)
            turtle.colorDoubleUnchecked(white)
            turtle.setVariable(index,(2*4*r.nextDouble()).toLogoObject)
            turtle.shape("sheep")
            turtle.xandycor(r.nextDouble()*world.getDimensions.width,r.nextDouble()*world.getDimensions.height)
            eContext.workspace.joinForeverButtons(turtle)
          }

          nvmContext.runExclusiveJob(world.breeds.get("SHEEP"), nvmContext.ip + 1)


          // if the optional command block wasn't supplied, then there's not
          // really any point in calling this, but it won't bomb, either
          //nvmContext.runExclusiveJob(agents.build(), nvmContext.ip + 1)
          // prim._extern will take care of leaving nvm.Context ip in the right place
        }

        def assemble(a: nvm.AssemblerAssistant): Unit = {
          a.block()
          a.done()
        }
      }

      class PreyAction extends Command with nvm.CustomAssembled {
        override def getSyntax = Syntax.commandSyntax(right = List(NumberType, NumberType, CommandBlockType | OptionalType))

        def perform(args: Array[api.Argument], context: api.Context): Unit = {
          implicit val myContext : api.Context = context
          val world = context.getAgent.world.asInstanceOf[agent.World]
          val eContext = context.asInstanceOf[nvm.ExtensionContext]
          val nvmContext = eContext.nvmContext
          implicit var t = eContext.getAgent.asInstanceOf[agent.Turtle]
          implicit val r = scala.util.Random

          move()
          consume(args(0).getDoubleValue)
          death()
          if (r.nextDouble() < (args(1).getDoubleValue*0.01)) reproduce()
        }

        def assemble(a: nvm.AssemblerAssistant): Unit = {
          a.block()
          a.done()
        }
      }

    }

  }


  feature("predModel") {
    trait MyPred {
      def move()(implicit context: api.Context, r: scala.util.Random, t: agent.Turtle): Unit
      def reproduce()(implicit context: api.Context, r: scala.util.Random, t: agent.Turtle): Unit
      def death()(implicit context: api.Context, t: agent.Turtle): Unit
      def consume(e: Double)(implicit context: api.Context, t: agent.Turtle): Unit

      def generateCommandPredAct(): PredatorAction = {
        new PredatorAction
      }

      class PredatorAction extends Command with nvm.CustomAssembled {
        override def getSyntax = Syntax.commandSyntax(right = List(NumberType, NumberType, CommandBlockType | OptionalType))

        def perform(args: Array[api.Argument], context: api.Context): Unit = {
          implicit val myContext: api.Context = context
          val world = context.getAgent.world.asInstanceOf[agent.World]
          val eContext = context.asInstanceOf[nvm.ExtensionContext]
          val nvmContext = eContext.nvmContext

          implicit var t = eContext.getAgent.asInstanceOf[agent.Turtle]
          implicit val r = scala.util.Random

          move()
          consume(args(0).getDoubleValue)
          death()
          if (r.nextDouble() < (args(1).getDoubleValue*0.01)) reproduce()
        }

        def assemble(a: nvm.AssemblerAssistant): Unit = {
          a.block()
          a.done()
        }
      }

    }

  }


  feature("patchModel")  {

  }

  feature("patchWithGrass") {
    trait MyPatch {

      def generateCommandSetUp(): PatchSetUp = {
        new PatchSetUp
      }

      def generateCommandGrow(): GrassGrow = {
        new GrassGrow
      }

      class PatchSetUp extends Command with nvm.CustomAssembled {
        override def getSyntax = Syntax.commandSyntax(right = List(NumberType, CommandBlockType | OptionalType))
        def perform(args: Array[api.Argument], context: api.Context): Unit = {
          //var pw = new PrintWriter(new File("/Users/yilmaz/IdeaProjects/example-scala/test.txt"))
          //pw.println("Patch with grass set up")
          val world = context.getAgent.world.asInstanceOf[agent.World]

          val eContext = context.asInstanceOf[nvm.ExtensionContext]
          val nvmContext = eContext.nvmContext
          var patchColor: String = null
          val p: Patch = eContext.getAgent.asInstanceOf[Patch]
          val r = scala.util.Random
          var index = world.patchesOwnIndexOf("COUNTDOWN")

          var grt: Double = args(0).getDoubleValue
          if (r.nextDouble() <= 0.5) {
            p.setVariable(2, Color.argbToColor(Color.getRGBByName("green")))
            patchColor = "green"
            p.setVariable(index, grt.toLogoObject)
          }
          else {
            p.setVariable(2, Color.argbToColor(Color.getRGBByName("brown")))
            patchColor = "brown"
            p.setVariable(index, (r.nextDouble() * grt).toLogoObject)
          }
          //pw.close()
        }

        def assemble(a: nvm.AssemblerAssistant) {
          a.block()
          a.done()
        }
      }


      class GrassGrow extends Command with nvm.CustomAssembled {
        override def getSyntax = Syntax.commandSyntax(right = List(NumberType, CommandBlockType | OptionalType))

        def perform(args: Array[api.Argument], context: api.Context): Unit = {
          val eContext = context.asInstanceOf[nvm.ExtensionContext]
          val world = context.getAgent.world.asInstanceOf[agent.World]
          val nvmContext = eContext.nvmContext
          val p: Patch = eContext.getAgent.asInstanceOf[Patch]
          if (p.pcolor==Color.argbToColor(Color.getRGBByName("brown"))) {
            var index = world.patchesOwnIndexOf("COUNTDOWN")
            if (p.getVariable(index).asInstanceOf[Double] <= 0) {
              p.setVariable(2, Color.argbToColor(Color.getRGBByName("green")))
              var grt: Double = args(0).getDoubleValue
              p.setVariable(index, grt.toLogoObject)
            }
            else
              p.setVariable(index,(p.getVariable(index).asInstanceOf[Double]-1).toLogoObject)
          }

        }

        def assemble(a: nvm.AssemblerAssistant): Unit = {
          a.block()
          a.done()
        }
      }


    }
  }

  feature("patchWithNoGrass") {
    trait MyPatch {
      def generateCommandSetUp(): PatchSetUp = {
        new PatchSetUp
      }

      class PatchSetUp extends Command with nvm.CustomAssembled {
        override def getSyntax = Syntax.commandSyntax(right = List(NumberType, CommandBlockType | OptionalType))
        def perform(args: Array[api.Argument], context: api.Context): Unit = {
          val world = context.getAgent.world.asInstanceOf[agent.World]
          val eContext = context.asInstanceOf[nvm.ExtensionContext]
          val nvmContext = eContext.nvmContext
          val p: Patch = eContext.getAgent.asInstanceOf[Patch]
          p.setVariable(2, Color.argbToColor(Color.getRGBByName("green")))
        }

        def assemble(a: nvm.AssemblerAssistant) {
          a.block()
          a.done()
        }
      }
    }
  }
}

*/