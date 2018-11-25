package metasim

import scala.meta._

object Collaboration {
  def feature(name: String)(body: => Unit): Unit = {
    body
  }

  def refines(body: => Unit): Unit = {
    body
  }
}



object FeatureGenerator {
  def apply(name: String)(featureSpec : Source = null, fileName: String = ""): FeatureImplementation = {
    new FeatureImplementation(name,featureSpec,fileName)
  }

}


class FeatureImplementation(var name: String, var body: Source, var fileName: String) {

}



