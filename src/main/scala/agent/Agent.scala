package agent

import java.awt.geom.Ellipse2D

import simulation.ModelUtility
import simulation.DTSim
import visualization._
import featuremodel._

import scala.xml._
import scala.collection.mutable.ListBuffer


/**
  * Created by yilmaz on 11/29/16.
  */





object Activity {
  def apply(cond: => Boolean)(body: => Unit)(exitCond: => Boolean): Activity = {
    new Activity(cond)(body) {
      override var agent : Agent = _
      def execute() : Unit = if (cond) body
      def completed() = exitCond
      def setAgent(a : Agent): Unit = {
        this.agent = a
      }
    }
  }
}

abstract class Activity(cond: => Boolean)(body: => Unit) {
  var agent : Agent
  def execute() : Unit
  def completed() : Boolean
  def condition() : Boolean = cond
  def setAgent(a : Agent)
}

trait ActivityAlgebra[E] {
  def Gen(cond: => Boolean)(body: => Unit)(exitCond: => Boolean) : E
  def Bind(e1 : E, e2 : E) : E
}

trait ActivityGenerator extends ActivityAlgebra[Activity] {
  def Gen(cond: => Boolean)(body: => Unit)(exitCond: => Boolean)  : Activity =
    new Activity(cond)(body) {
      override var agent : Agent = _
      def execute() : Unit = if (cond) body
      def completed() = exitCond
      def setAgent(a : Agent): Unit = {
        this.agent = a
      }
    }


  def Bind(e1: Activity, e2: Activity) : Activity =
    new Activity(e1.condition())({}) {
      override var agent : Agent = _
      def execute() : Unit = {
        e1.execute()
        e2.execute()
      }
      def completed() = e2.completed()
      def setAgent(a : Agent): Unit = {
        this.agent = a
      }
    }
}



trait Component {
  type ActivityQueue = scala.collection.mutable.Queue[Activity]
  type MessageQueue = scala.collection.mutable.Queue[scala.xml.Node]
  var state : scala.xml.Node = _
  var dataModel : State = _
  def action() : Unit
  def delete() : Unit
}


class Agent extends Component {
  var agentId : Double =_
  var agentType : String = _
  var bQueue : ActivityQueue = scala.collection.mutable.Queue[Activity]()
  var mQueue : MessageQueue = scala.collection.mutable.Queue[scala.xml.Node]()
  var messageProcessor : Activity = _


  def addActivity(a: Activity) : Unit = {
    bQueue.enqueue(a)
  }

  def delete(): Unit = {
    ModelUtility.remove(this)
  }

  def send(target : Agent, m : scala.xml.Node): Unit = {
    target.mQueue.enqueue(m)
  }

  def receive(): scala.xml.Node = {
    var nextMessage : scala.xml.Node = null
    if (mQueue.nonEmpty)
      nextMessage = mQueue.dequeue()
    nextMessage
  }

  def update(m : scala.xml.Node): Unit = {

  }


  def action() : Unit = {
    if (messageProcessor != null) messageProcessor.execute()
    val notCompleted : scala.collection.mutable.ListBuffer[Activity] = ListBuffer()
    while  (bQueue.nonEmpty) {
      val myA = bQueue.dequeue()
      myA.execute()
      if (!myA.completed) notCompleted += myA
    }
    val y = notCompleted.toList
    bQueue ++= y
    if (bQueue.isEmpty) delete()
  }

}

abstract class Message {
  var description : String
  var sender : Int


  def toXML =
    <message>
      <description>{description}</description>
      <agent>{sender}</agent>
    </message>

}

abstract class State {
  var description : String
  override def toString: String = description
}


abstract class Holon extends Component {
  var elements : List[Component]
}



trait AgentAlgebra[S <: Component] {
  def Cons(x : List[Activity]) : S
  def Compose(e1 : S, e2 : S) : S
}


trait AgentGenerator extends AgentAlgebra[Component] {
  def Cons(aList: List[Activity])  : Agent = {
    new Agent {
      for (x <- aList) addActivity(x)
      ModelUtility.ctx.add(this)
    }

  }

  def Cons(f: Feature) : Agent = {
    new Agent {
      self =>
      f.agent = self
      for (x <- f.behavior) {
        addActivity(x)
      }
      ModelUtility.ctx.add(self)
    }
  }

  def Cons(): Agent = {
    new Agent {
      ModelUtility.ctx.add(this)
    }
  }

  def Cons(s : scala.xml.Node): Agent = {
    new Agent {
      this.state = s
      agentType = this.state.\("agentType").text
      ModelUtility.ctx.add(this)

    }

  }

  def Compose(c1: Component, c2: Component): Holon = {
    new Holon {
      var elements = List(c1, c2)

      def action(): Unit = {

      }

      def delete(): Unit = {
        elements = null
      }
    }
  }
}