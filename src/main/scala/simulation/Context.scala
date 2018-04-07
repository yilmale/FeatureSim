package simulation

import agent._
import visualization.{AgentSpaceContainer, GridVis, Space2D, SpaceVis}

import scala.collection.mutable.ListBuffer

/**
  * Created by Levent Yilmaz on 2/4/2017.
  */
trait Context  {
  var agentIdentifier : Double = 0
  var simulator : DTSim = _
  var simAgents = scala.collection.mutable.ListBuffer.empty[Agent]
  var simView : Grid = _
  var simSpace2D : SpaceVis = _
  var projectionDefined : Boolean = false
  var spaceDefined : Boolean = false
  var simExperiment : Experiment = ModelUtility.experiment
  var model : Model = _
  var monitor : Agent with Observer = _
  ModelUtility.register(this)


  def setProjection(g: Grid): Unit = {
    ModelUtility.setProjection(g)
    projectionDefined = true
    simView = g

  }

  def setProjection(s : SpaceVis): Unit = {
    ModelUtility.setSpace(s)
    spaceDefined = true
    simSpace2D = s

  }

  def setExperiment(e : Experiment): Unit = {
    simExperiment = e
  }

  def param(pname : String): Any = {
    simExperiment.params(pname)
  }


  def setModelName(n : String): Unit = {
    ModelUtility.setModelName(n)
  }

  def add(agent : Agent): Unit = {
    agent.agentId=agentIdentifier
    agentIdentifier=agentIdentifier+1
    simAgents += agent
    if (projectionDefined) {
      val x,y = scala.util.Random
      val xcoord = x.nextInt(ModelUtility.gridView.XDim)
      val ycoord = y.nextInt(ModelUtility.gridView.YDim)
      ModelUtility.gridView.grid(xcoord)(ycoord) += agent
      ModelUtility.gridView.location += (agent -> (xcoord,ycoord))
    }
    if (spaceDefined) {
      val r = scala.util.Random
      val xcoord = r.nextInt(ModelUtility.spaceView.xDim)
      val ycoord = r.nextInt(ModelUtility.spaceView.yDim)
      ModelUtility.spaceView.add(
        new AgentSpaceContainer(agent,xcoord,ycoord))
    }

  }

  def add(agent : Agent, x : Int, y : Int): Unit = {
    agent.agentId=agentIdentifier
    agentIdentifier=agentIdentifier+1
    simAgents += agent
    if (projectionDefined) {
      ModelUtility.gridView.grid(x)(y) += agent
      ModelUtility.gridView.location += (agent -> (x,y))
    }

    if (spaceDefined) {
      ModelUtility.spaceView.add(
        new AgentSpaceContainer(agent,x,y))
    }

  }

  def remove(agent: Agent): Unit = {
    simAgents -= agent
    simulator remove agent


  }

  def simulate(): Unit = {
    if (monitor != null) add(monitor)
    initSim(param("stopTime").asInstanceOf[Int]).run()
  }

  private def initSim(st : Int) : DTSim = {
    simulator = new DTSim {
      stopTime = st

      override def init(): Unit = {
        for (x <- simAgents) {
          var e = new EventWrapper { self =>
            target = x
            event = new Function1[Agent,Unit] {
              def apply(x:Agent): Unit = {
                x.action()
                schedule(1,self)
              }
            }
          }
          schedule(0,e)
        }
      }
    }
    simulator
  }


}


class EventWrapper {
  var target : Agent = _
  var event : Function[Agent,Unit] = _

}

class Grid {
  var grid : Array[Array[ListBuffer[Agent]]] = _
  var location : collection.mutable.Map[Agent,(Int,Int)] = _
  var vis : GridVis = _
  var XDim : Int = _
  var YDim : Int = _

  def getXDim : Int = XDim
  def getYDim : Int = YDim

  def setGridVis(gvis : GridVis): Unit = {
    vis = gvis
  }

  def remove(a : Agent): Unit = {
    val (p,q) = location(a)
    grid(p)(q) -= a
    location - a
  }

  def move(a: Agent, x : Int, y: Int): Unit = {
    val (p,q) = location(a)
    grid(p)(q) -= a
    grid(x)(y) += a
    location += (a -> (x,y))
  }

  def update(): Unit = {

  }
}



trait GridView  {
  def createGridView(x : Int, y : Int) : Grid = {
    var g = new Grid {
      XDim=x
      YDim=y
      grid = Array.ofDim[ListBuffer[Agent]](x, y)
      for (i <- 0 until x)
        for (j <- 0 until y) {
          grid(i)(j) = scala.collection.mutable.ListBuffer.empty[Agent]
        }
      location = collection.mutable.Map[Agent,(Int,Int)]()

    }
    ModelUtility.setProjection(g)
    g
  }

  def createGridVis(g: Grid) : Unit = {
    g setGridVis GridVis.createGridVis(g.XDim,g.YDim)
  }

  def move(a : Agent, x : Int, y: Int): Unit = {
    ModelUtility.gridView.move(a,x,y)
  }

}