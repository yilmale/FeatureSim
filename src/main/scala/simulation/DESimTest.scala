package simulation

/**
  * Created by Levent Yilmaz on 11/28/2016.
  */
object DESimTest {
  def apply(): Unit = {
    val mySim = new DESim {
      var x: Int = 0
      stopTime=20
      override def init(): Unit = {
        schedule(0) {
          println("Simulation started, time = " + currentTime + " ***")
          //schedule(3) {event1()}
        }

        schedule(0) {event1()}
        schedule(0) {event2()}
        schedule(0) {event3()}

      }

      def event1(): Unit = {
        x = x + 1
        println("Event 1 executed, x = " + x + " at time = " + currentTime)
        schedule(2) {
          event2()
        }
      }
      def event2(): Unit = {
        x = x + 1
        println("Event 2 executed, x = " + x + " at time = " + currentTime)
        schedule(5) {
          event3()
        }
      }

      def event3(): Unit = {
        x = x + 1
        println("Event 3 executed, x = " + x + " at time = " + currentTime)

      }

    }

    mySim.run()
  }

}