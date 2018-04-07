package simulation

/**
  * Created by yilmaz on 10/13/16.
  */



abstract class DESim {
  type Action = () => Unit
  var stopTime : Int = _
  case class SimEvent(time: Int, action: Action)

  var curtime = 0

  def currentTime: Int = curtime

  var eventList: List[SimEvent] = List()

  private def insert(evL: List[SimEvent], event:SimEvent) : List[SimEvent] = {
    if (evL.isEmpty || event.time < evL.head.time) event :: evL
    else evL.head :: insert(evL.tail,event)
  }

  def schedule(delay: Int)(block: => Unit) = {
    val newEvent = SimEvent(currentTime + delay, () => block)
    eventList = insert(eventList,newEvent)

  }

  def next(): Unit = {
    (eventList: @unchecked) match {
      case item :: rest =>
        eventList = rest
        curtime = item.time
        item.action()
    }
  }

  def init() : Unit

  def run(): Unit = {
    init()
    while (!eventList.isEmpty && currentTime<=stopTime) next()

  }

}