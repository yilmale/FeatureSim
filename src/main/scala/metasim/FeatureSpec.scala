package metasim

import scala.meta._
import java.io._

import coherence.CoherenceModel
import FeatureComposer._

import scala.collection.mutable


abstract class FeatureExpression {var fname: String}
case class And(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Or(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Base(name:String) extends FeatureExpression {var fname = name}
case class Feature(name: String) extends FeatureExpression {var fname = name}
case class Optional(name: String, children: List[FeatureExpression]=null) extends FeatureExpression {var  fname = name}
case class Xor(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class FeatureTree(node: FeatureExpression,
                       children: List[FeatureExpression] = List[FeatureExpression]())
case class ResolutionModel(rm: scala.collection.mutable.Map[String,Boolean])


object FeatureSpecifications {
  def apply(fileName: String): scala.meta.Source = {
    var features : String = ""
    scala.io.Source.fromFile(fileName) foreach {x =>
      features = features + x
    }
    features.parse[scala.meta.Source].get
  }
}


abstract class VariabilityModel {
  var features : scala.meta.Source
  var featureTree : FeatureTree
  var featureCoherenceGraph : CoherenceModel
  var resolution : ResolutionModel
  var featureModules : scala.collection.mutable.Map[String,Defn.Object]


  def compileFeatureTree(fileName: String): Defn.Object = {
    val fs = FeatureTreeEvaluator(featureTree,resolution)
    var composite = FeatureComposer(features,fs)
    val writer = new PrintWriter(new File(fileName))

    writer.write(composite.toString())
    writer.close()
    composite
  }

  def compile(fileName: String, fs: Array[String]) : Defn.Object = {
    var composite = FeatureComposer(features,fs)
    val writer = new PrintWriter(new File(fileName))

    writer.write(composite.toString())
    writer.close()
    composite
  }

  def compileFeatures(): Unit = {
    var fts = transform(features)
    fts foreach { f =>
      {
        featureModules += (f.name.toString -> f)
      }

    }
  }

  def compileCoherenceModel(fileName: String): Defn.Object = {
    var rmod = featureCoherenceGraph.evaluate().generateResolution()
    val fs = featureCoherenceGraph.serialize(rmod)
    var composite = FeatureComposer(features,fs)
    val writer = new PrintWriter(new File(fileName))

    writer.write(composite.toString())
    writer.close()
    composite
  }

}


object VariabilityModel {

  /*
   def apply(x: scala.meta.Source)(y: FeatureTree)(z: ResolutionModel): VariabilityModel = {
     new VariabilityModel {
       override var features: Source = x
       override var featureTree: FeatureTree = y
       override var resolution: ResolutionModel = z
       override var featureCoherenceGraph: CoherenceModel = _
     }
   }*/

  def apply(x: scala.meta.Source)(y: CoherenceModel) : VariabilityModel = {
    new VariabilityModel {
      override var features: Source = x
      override var featureTree: FeatureTree = _
      override var resolution: ResolutionModel = _
      override var featureCoherenceGraph: CoherenceModel = y
      override var featureModules = mutable.Map[String, Defn.Object]()
    }
  }

}

object FeatureTreeEvaluator {
  type RM = scala.collection.mutable.Map[String,Boolean]
  var composite : String = ""
  var featureModel : FeatureTree = null
  var resolution: RM = null
  def apply(fm: FeatureTree,res: ResolutionModel): Array[String] = {
    featureModel = fm
    resolution = res.rm
    compile()
  }

  def compile(): Array[String] = {
    evaluate(featureModel)
    composite split("_")
  }




  def evaluate(x: FeatureTree): Unit = {

    x.node match {
      case f : Feature => {composite = composite + "_" + f.name}
      case b : Base => {
        composite = composite + b.name
        x.children foreach { c =>
          evaluate(FeatureTree(c))
        }
      }
      case op : Optional => {
        if ((resolution.keys.exists(x => (x == op.fname) && (resolution(op.fname) == true)))) {
          op.children foreach { c =>
            evaluate(FeatureTree(c))
          }
        }
      }
      case a : And => {
        composite = composite + "_" + a.name
        a.children foreach { c =>
           evaluate(FeatureTree(c))
        }
      }
      case o : Or => {
        composite = composite + "_" + o.name
        o.children foreach { c =>
        {
          if ((resolution.keys.exists(x => (x == c.fname) && (resolution(x) == true)))) {
             evaluate(FeatureTree(c))
          }
        }
        }
      }
      case x : Xor => {
        var found = false
        composite = composite + "_" + x.name
        x.children foreach { c =>
          if (found==false) {
            if ((resolution.keys.exists(x => (x == c.fname) && (resolution(x) == true)))) {
              found = true
              evaluate(FeatureTree(c))
            }
          }
        }
      }
    }
  }
}