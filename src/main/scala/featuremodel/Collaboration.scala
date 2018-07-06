package featuremodel

object Collaboration {
  def feature(name: String)(body: => Unit): Unit = {
    body
  }

  def refines(body: => Unit): Unit = {
    body
  }
}



package a {

  abstract class Graph {
    def print(): Unit = {
      var x = 5
    }
  }


  abstract class Node {

  }


  abstract class Edge {

  }

}

package b {

  import Collaboration._

  object feature {
    refines {
      class Graph {

      }
    }

    refines {
      class Edge(n: a.Node) {

      }
    }

    refines {
      class Node
    }

    trait Weight {

    }
  }

}







class Collaboration {
  import Collaboration._
  val f = feature("basic") {
    class Graph {
      val u : Int = 0
    }

    class Edge(na: Node, nb: Node) {
      def print(): Unit = {

      }

    }

    class Node {
      var n : Double = 0.0
    }
  }

  val t = feature("weighted") {
    refines {
      class Graph {

      }
    }

    refines {
      class Edge {
        var w : Weight = null
        def print(): Unit = {

        }
      }
    }

    class Weight {
      var w: Double = 0.0
    }

  }
}
