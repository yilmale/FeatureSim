package metasim

import org.nlogo.api.ScalaConversions._
import org.nlogo.core.Syntax
import org.nlogo.core.Syntax.{ListType, NumberType, CommandBlockType, OptionalType,RepeatableType, WildcardType}
import org.nlogo.{agent, api, core, nvm}
import org.nlogo.api._
import java.io._

import Collaboration._
object FeatureModel {

  feature("base") {
    class MyPatch {
      val posx : Int = 10
      val posy : Int = 20
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
}

