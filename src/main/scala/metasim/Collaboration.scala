package metasim

object Collaboration {
  def feature(name: String)(body: => Unit): Unit = {
    body
  }

  def refines(body: => Unit): Unit = {
    body
  }
}


object FMTest {
  import Collaboration._
  class PreyActivity {

  }

  trait A_PreyActivity {
    def move() : Unit
    def genCmd() : Cmd = {
      new Cmd()
    }
    class Cmd {
      move()
    }
  }

  trait B_PreyActivity {
    def move(): Unit = {
      println("B_PreyActivity move")
    }
  }

  class Prey extends PreyActivity with A_PreyActivity with B_PreyActivity

  def apply(): Unit = {
    new Prey() genCmd()
  }




  feature("PreyActivity") {
    trait A_PreyActivity {}
  }
}




