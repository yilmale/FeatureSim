package featuremodel

import agent._

/**
  * Created by yilmaz on 1/9/17.
  */



trait FeatureAlg[E] {
  var featureName : String
  val featureSet : Set[String]
  def Def(x:List[Activity]) : E
  def Cons() : E
  def Cons(a : Agent) : E
}


trait Lifter[A,B] {
  def lift(x : A, y : B ) : A with B
}

trait GenericLifter[A] {
  def lift(x: A, y: A) : A
}

class MkLifter[A,B](f : (A,B) => A with B) extends Lifter[A,B] {
  def lift(x : A, y : B) : A with B = f(x,y)
}


trait FeatureMerge[A,B] extends FeatureAlg[A] {
  val lifter : Lifter[A,B]
  val alg1 : FeatureAlg[A]
  val alg2 : FeatureAlg[B]


  def Def(x : List[Activity], y : List[Activity]) : A with B = {
    lifter.lift(alg1.Def(x), alg2.Def(y))
  }

  def Def(x: List[Activity]): A with B = {
    lifter.lift(alg1.Def(x), alg2.Def(x))
  }

  def Cons() : A with B = {
    lifter.lift(alg1.Cons(), alg2.Cons())
  }

  def Cons(a : Agent) : A with B = {
    lifter.lift(alg1.Cons(a), alg2.Cons(a))
  }

}

trait Feature {
  var agent : Agent
  var behavior : List[Activity]
  def Val() : List[Activity]
  def eVal() : Unit
}

trait BaseModel extends Feature {
  var featureName : String
}



trait BaseModelGen extends FeatureAlg[Feature] {
  var featureName = "base"
  val featureSet: Set[String] = Set()

  def Def(x: List[Activity]): Feature = new BaseModel {
    var featureName = "base"
    var agent : Agent =null
    var behavior = x

    def Val(): List[Activity] = {
      behavior
    }

    def eVal(): Unit = {
      for (x <- behavior) x.execute()
    }
  }



  def Cons(): Feature = new BaseModel {
    var agent : Agent = null
    var featureName = "base"
    var behavior: List[Activity] = null

    def Val(): List[Activity] = {
      behavior
    }

    def eVal(): Unit = {

    }
  }

  def Cons(a : Agent): Feature = new BaseModel {
    var agent : Agent = a
    var featureName = "base"
    var behavior: List[Activity] = null

    def Val(): List[Activity] = {
      behavior
    }

    def eVal(): Unit = {

    }
  }
}




object ag extends ActivityGenerator
object base extends BaseModelGen



object baseGenerator extends FeatureMerge[Feature,Feature] {
  var featureName = "base"
  val featureSet : Set[String] = Set()
  val alg1 = base
  val alg2 = base
  val lifter = LiftBase
}

object LiftBase extends Lifter[Feature,Feature] {
  def lift(x : Feature, y : Feature = null) = new BaseModel {
    var featureName = "base"
    var agent : Agent = null
    var behavior : List[Activity] =  x.behavior

    def eVal() = x.eVal()
    def Val() : List[Activity] = x.Val()
  }
}


trait VariabilityModel {
  val featureModel : FModel
  val features : Map[String, Object]
  val resolution : Map[String, Boolean]
  val composer : Map[(String,String),FeatureMerge[Feature,Feature]]
  val composer1 : Map[(Set[String],Set[String]),FeatureMerge[Feature,Feature]]
  val base : Map[String,Lifter[Feature,Feature]]
  val baseBehavior : BaseModelGen
}