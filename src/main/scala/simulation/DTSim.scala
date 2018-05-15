package simulation

import agent._
import scala.collection.mutable.ListBuffer

/**
  * Created by Levent Yilmaz on 10/14/2016.
  */

abstract class DTSim {
  type Action = (Agent) => Unit
  var stopTime : Int = _
  case class SimEvent(time: Int, action: Action, agent : Agent)
  var removeList : List[Agent] = List()
  var curtime = 0

  def currentTime: Int = curtime

  var eventList: List[SimEvent] = List()

  private def insert(evL: List[SimEvent], event:SimEvent) : List[SimEvent] = {
    if (evL.isEmpty || event.time < evL.head.time) event :: evL
    else evL.head :: insert(evL.tail,event)
  }

  def remove(a : Agent): Unit = {
    removeList = a :: removeList
  }



  def schedule(delay: Int, ew : EventWrapper) = {
    val newEvent = SimEvent(currentTime + delay, ew.event, ew.target)
    eventList = insert(eventList,newEvent)

  }

  def next(): Unit = {
    (eventList: @unchecked) match {
      case item :: rest =>
        eventList = rest
        curtime = item.time
        item.action(item.agent)

    }
  }

  def init() : Unit

  private def shuffle(x: List[SimEvent]) : List[SimEvent] = {
    var newEList = ListBuffer.empty[SimEvent]
    var rIndex: Int = 0;
    var tValue: SimEvent = null
    val r = scala.util.Random

    for (sevt <- x ) {
      newEList += sevt
    }

    var cIndex: Int = newEList.length


    for (i <- cIndex-1 to 1 by -1) {
      rIndex = r.nextInt(i+1)
      tValue = newEList(rIndex)
      newEList(rIndex) = newEList(i)
      newEList(i)=tValue
    }

    newEList.toList
  }

  def readyEvents() : List[SimEvent] = {
    var rList: List[SimEvent] = List()
    while (!eventList.isEmpty && eventList.head.time<=currentTime) {
      val x = eventList.head
      rList = x :: rList
      eventList=eventList.tail
    }
    rList
  }

  def execute(s : List[SimEvent]): Unit = {
    for (n <- s) {
      n.action(n.agent)
    }
  }

  def update(): Unit = {
    var tempList = ListBuffer.empty[SimEvent]
    for (x <- eventList) {
      if  (!(removeList.contains(x.agent)))
        tempList += x
    }

    eventList = tempList.toList
    removeList = List()
  }

  def run(): Unit = {
    init()
    while (!eventList.isEmpty && currentTime<=stopTime) {
      execute(shuffle(readyEvents()))
      update()
      curtime=curtime+1
    }

  }

}