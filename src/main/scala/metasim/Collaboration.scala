package metasim

object Collaboration {
  def feature(name: String)(body: => Unit): Unit = {
    body
  }

  def refines(body: => Unit): Unit = {
    body
  }
}



object FeatureGenerator {
  def apply(name: String)(body: => Unit): FeatureImplementation = {
    new FeatureImplementation(name,body)
  }

}


class FeatureImplementation (name: String, body: => Unit) {
  def execute(): Unit = {
    body
  }
}



