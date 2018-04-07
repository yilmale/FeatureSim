package simulation

/**
  * Created by Levent Yilmaz on 11/28/2016.
  */
object DTSimTest {
  def apply(): Unit = {
    val mytSim = new DTSim {
      var x: Int = 0
      stopTime  = 20
      /*
            def event1(): Unit = {
              x = x + 1
              println("Event 1 executed, x = " + x + " at time = " + currentTime)
              schedule(1) {
                event1()
              }
            }
            def event2(): Unit = {
              x = x + 1
              println("Event 2 executed, x = " + x + " at time = " + currentTime)
              schedule(1) {
                event2()
              }
            }
            def event3(): Unit = {
              x = x + 1
              println("Event 3 executed, x = " + x + " at time = " + currentTime)
              schedule(1) {
                event3()
              }
            }
      */
      override def init(): Unit = {
        /*schedule(0) {
          println("Simulation started, time = " + currentTime + " ***")
          event1()
          event2()
          event3()*/
      }


    }
  }

  //mytSim.run()


}