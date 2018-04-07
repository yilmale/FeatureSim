package predation

import agent._
import simulation._


/**
  * Created by yilmaz on 12/2/16.
  */
class Prey extends Agent {self=>
  var caught: Boolean = false
  var g = ModelUtility.gridView


  object PreyRG extends ActivityGenerator

  val move = PreyRG.Gen(true) {
    var cX = g.location(this)._1
    var cY = g.location(this)._2
    var r = scala.util.Random
    var direction: Double = r.nextDouble()
    var bound : Int = 2
    var dx = r.nextInt(bound)
    if (direction < 0.5) dx = -dx
    var dy = r.nextInt(bound)
    direction = r.nextDouble()
    if (direction < 0.5) dy = -dy
    cX = math.abs(cX + dx) % g.getXDim
    cY = math.abs(cY + dy) % g.getYDim
    g.move(this, cX, cY)
  }(false)

  addActivity(move)

  val death = PreyRG.Gen(true) {
    if (caught) {
      println("This prey was caught....")
      ModelUtility.remove(self)
    }
  }(false)

  addActivity(death)

  val reproduction = PreyRG.Gen(true) {
    var r = scala.util.Random
    var dP = r.nextDouble()
    if (dP <= ModelUtility.experiment.params("reproductionRate").asInstanceOf[Double]) {
      var cX = g.location(this)._1
      var cY = g.location(this)._2
      ModelUtility.ctx.add(new Prey(), cX, cY)
    }
  }(false)

  addActivity(reproduction)
}


class Predator extends Agent {self=>
  var id : Int = _
  var g = ModelUtility.gridView
  agentType = "Predator"

  object PredRG extends ActivityGenerator

  val move = PredRG.Gen(true) {
    var cX = g.location(this)._1
    var cY = g.location(this)._2
    var r = scala.util.Random
    var direction : Double = r.nextDouble()
    var bound : Int = 2
    var dx = r.nextInt(bound)
    if (direction < 0.5) dx = -dx
    var dy = r.nextInt(bound)
    direction = r.nextDouble()
    if (direction < 0.5) dy = -dy
    cX = math.abs(cX + dx) % g.getXDim
    cY = math.abs(cY + dy) % g.getYDim
    g.move(this,cX,cY)
  } (false)

  addActivity(move)

  val remove = PredRG.Gen(true) {
    var r = scala.util.Random
    var dP = r.nextDouble()
    if (dP <= ModelUtility.experiment.params("predDeathProb").asInstanceOf[Double])
      ModelUtility.remove(self)


  } (false)

  addActivity(remove)

  val predation = PredRG.Gen(true) {
    var cX = g.location(this)._1
    var cY = g.location(this)._2
    var agentList = g.grid(cX)(cY)
    var r = scala.util.Random
    var bound : Int = agentList.size
    if (bound > 1) {
      println("there are " + bound + "agents ....")
      val selected = r.nextInt(bound)
      if (agentList(selected).agentType == "Prey") {
        var msg = <caught>true</caught>
        send(agentList(selected),msg)
        var dP = r.nextDouble()
        if (dP <= ModelUtility.experiment.params("predReproduce").asInstanceOf[Double]) {
          ModelUtility.ctx.add(new Predator(), cX, cY)
        }
      }
    }

  } (false)

  addActivity(predation)
}

trait PredationAnalysis extends AnalysisModel {
  def analyze: Unit = {

  }
}

class PredationMonitor extends Agent with Observer {
  var g = ModelUtility.gridView
  override var monitors: Map[String,Any] = Map("preyCount" -> 0, "predCount" -> 0)
  var preyCount = 0
  var predatorCount = 0

  object MonitorRG extends ActivityGenerator

  val preypredCounter = MonitorRG.Gen(true) {
    var count1 = 0
    var count2 = 0
    for ( x <- ModelUtility.ctx.simAgents) {
      if (x.agentType == "Predator") {
        count1=count1 + 1
      }
      else
      if  (x.agentType == "Prey") {
        count2 = count2 + 1
      }
    }
    preyCount = count2
    predatorCount = count1
    count1 = 0
    count2 = 0
    monitor()

  } (false)

  addActivity(preypredCounter)


  def monitor(): Unit = {
    println("The prey count is....................... " + preyCount)
    println("The predator count is........................ " + predatorCount)
  }

}