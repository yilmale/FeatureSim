package norm

import agent.{ActivityGenerator, Agent}
import simulation.ModelUtility

/**
  * Created by yilmaz on 8/1/17.
  */
class NormativeAgent() extends Agent {
  var s = ModelUtility.spaceView
  agentType = "Normative"

  object NormRG extends ActivityGenerator

  val move = NormRG.Gen(true) {
    var cX = s.locations(this)._1
    var cY = s.locations(this)._2
    println("Agent " + this.agentId + " at location " + "(" + cX + "," + cY + ") is active")
    var r = scala.util.Random
    var direction : Double = r.nextDouble()
    var bound : Int = 20
    var dx = r.nextInt(bound)
    if (direction < 0.5) dx = -dx
    var dy = r.nextInt(bound)
    direction = r.nextDouble()
    if (direction < 0.5) dy = -dy
    s.moveBy(this,dx,dy)
  } (false)

  addActivity(move)
}