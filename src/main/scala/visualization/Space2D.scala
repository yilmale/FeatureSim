package visualization

import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.geom.Ellipse2D

import agent.{Agent}
import simulation.ModelUtility

/**
  * Created by yilmaz on 7/31/17.
  */
class Space2D(x: Int, y: Int) {
  var xDim = x
  var yDim = y
  def createSpace2D() : SpaceVis = {
    val space = new SpaceVis(xDim, yDim)
    val frame = new JFrame("Space2D")
    //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.add(space)
    frame.setSize(xDim, yDim)
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
    ModelUtility.setSpace(space)
    space
  }

}


class SpaceVis(x: Int, y: Int) extends JPanel {
  var xDim = x
  var yDim = y
  var locations = collection.mutable.Map[Agent,(Int,Int)]()
  var agentMapper = collection.mutable.Map[Agent,AgentSpaceContainer]()
  var eList = List[AgentSpaceContainer]()

  def add(e: AgentSpaceContainer): Unit = {
    eList = e :: eList
    locations += (e.myAgent -> (e.positionX,e.positionY))
    agentMapper += (e.myAgent->e)
  }

  def remove(a: Agent): Unit = {
    var e : AgentSpaceContainer = agentMapper(a)
    eList = eList filter (_ != e)
    locations = locations - a
  }

  def update(): Unit = {
    paintComponent(this.getGraphics())
  }

  def moveBy(a: Agent, dx: Int, dy: Int): Unit = {
    var g2d = getGraphics.asInstanceOf[Graphics2D]
    g2d.setColor(Color.RED)
    var ac : AgentSpaceContainer = agentMapper(a)
    ac.positionX += dx
    ac.positionY += dy
    locations(a)=((ac.positionX % yDim),(ac.positionY % xDim))
    ac.myShape = new Ellipse2D.Double((ac.positionX % xDim), (ac.positionY % yDim), 10, 10)
    update()
  }

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    repaint()
    var g2d = g.asInstanceOf[Graphics2D]
    g2d.setColor(Color.RED)
    for (s <-eList) {
      g2d.fill(s.myShape)
      g2d.drawString(s.myId,s.positionX % xDim,s.positionY % yDim)
    }
  }
}

class Entity(x: Int, y: Int, id: Int) {
  var positionX = x
  var positionY = y
  var myShape = new Ellipse2D.Double(x, y, 10, 10)
  var myId : String = id.toString()


  def move(s: SpaceVis, dx: Int, dy: Int): Unit = {
    var g2d = s.getGraphics.asInstanceOf[Graphics2D]
    g2d.setColor(Color.BLUE)
    positionX += dx
    positionY += dy
    myShape = new Ellipse2D.Double(positionX % 500, positionY % 500, 10, 10)
  }

}

class AgentSpaceContainer (a : Agent, x: Int, y: Int) {
  var myAgent : Agent = a
  var positionX = x
  var positionY = y
  var myShape = new Ellipse2D.Double(x, y, 10, 10)
  var myId : String = a.agentId.toString()
}