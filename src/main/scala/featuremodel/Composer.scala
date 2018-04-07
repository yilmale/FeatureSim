package featuremodel

import agent.{Activity, Agent}

import scala.collection.mutable



/**
  * Created by yilmaz on 1/5/17.
  */


abstract class FOM {
  val subFeatures : List[FOM]
  val name : String
}

case class FModel(name:String,subFeatures: List[FOM]) extends FOM
case class F(name:String, subFeatures:List[FOM]=Nil) extends FOM
case class And(name: String, subFeatures:List[FOM]) extends FOM
case class Or(name: String, subFeatures:List[FOM]) extends FOM
case class Xor(name: String, subFeatures:List[FOM]) extends FOM
case class Optional(name: String, subFeatures:List[FOM]) extends FOM


trait FeatureComposer {
  var featureModel : FModel = _
  var baseBehavior : BaseModelGen = _
  var featureMap = Map[String, Object]()
  var resModel = Map[String, Boolean]()
  var fLMap = Map[(String,String),FeatureMerge[Feature,Feature]]()
  var fSMap = Map[(Set[String],Set[String]),FeatureMerge[Feature,Feature]]()
  var baseLMap = Map[String,Lifter[Feature,Feature]]()
  def evaluate(fMod: List[FOM],base : BaseModelGen): FeatureMerge[Feature,Feature]
  def evaluate() : FeatureMerge[Feature,Feature]
}

object FOModel extends FeatureComposer  {


  def apply(vm : VariabilityModel) : FeatureMerge[Feature,Feature] = {
    featureMap = vm.features
    resModel = vm.resolution
    fLMap = vm.composer
    fSMap = vm.composer1
    baseLMap = vm.base
    featureModel = vm.featureModel
    baseBehavior = vm.baseBehavior
    evaluateGeneric(featureModel.subFeatures,baseBehavior)
    //evaluate(featureModel.subFeatures,baseBehavior)
  }

  def apply(fMap : Map[String, Object], resolution : Map[String, Boolean],
            fMerger : Map[(String,String),FeatureMerge[Feature,Feature]],
            baseLifter : Map[String,Lifter[Feature,Feature]]): FeatureComposer = {
    featureMap = fMap
    resModel = resolution
    fLMap = fMerger
    baseLMap = baseLifter
    this
  }

  def evaluate(): FeatureMerge[Feature,Feature] = {
    evaluate(featureModel.subFeatures,baseBehavior)
  }

  def evaluateGeneric(fMod: List[FOM], base : BaseModelGen) : FeatureMerge[Feature,Feature] = {
    println("Model is " + fMod)
    var fm : FeatureMerge[Feature,Feature] = null
    var fmp1 : FeatureMerge[Feature,Feature] = null
    var fmp2 : FeatureMerge[Feature,Feature] = null


    fMod match {
      case h::t =>
        println("head: " + h)
        h match {
          case F(name,_) =>
            println("Plain feature")
            val f1g = featureMap(name).asInstanceOf[FeatureAlg[Feature]]
            fmp1 = new FeatureMerge[Feature,Feature] {
              var featureName = f1g.featureName
              val alg1 = f1g
              val alg2 = base
              val featureSet = f1g.featureSet
              val lifter = new Lifter[Feature,Feature] {
                def lift(x : Feature, y : Feature) = new Feature  {
                  def Val() : List[Activity] = x.behavior
                  def eVal() : Unit =  {
                    for (x <- behavior) x.execute()
                  }
                  override var agent : Agent = null
                  override var behavior : List[Activity] = x.behavior
                }
              }
            }

          case And(_,_) =>
            println("And operator")
            val hd = h.subFeatures.head
            val rest = h.subFeatures.tail
            fmp1 = evaluate(List(hd),base)
            for (x <- rest) {
              fmp2 = evaluate(List(x),base)
              fmp1 = new FeatureMerge[Feature,Feature] {
                var featureName = fmp1.featureName + fmp2.featureName
                val alg1 = fmp1
                val alg2 = fmp2
                val featureSet = fmp1.featureSet ++ fmp2.featureSet
                val lifter = new Lifter[Feature,Feature] {
                  def lift(x : Feature, y : Feature) = new Feature  {
                    def Val() : List[Activity] = x.behavior
                    def eVal() : Unit =  {
                      for (x <- behavior) x.execute()
                    }
                    override var agent : Agent = null
                    override var behavior : List[Activity] = x.behavior union y.behavior
                  }
                }
              }
            }
            println("And operation applied - " + "merged to create " + fmp1.featureName)

          case Or(_,_) =>
            println("Or operator")
            var found : Boolean = false
            var completed : Boolean = false
            val fList = h.subFeatures.iterator
            while ((fList.hasNext) && (!completed)) {
              val hd = fList.next()
              if ((resModel(hd.name)==true) && (found==false)) {
                fmp1 = evaluate(List(hd),base)
                found=true
              }
              else
              if ((resModel(hd.name)==true) && (found==true)) {
                fmp2 = evaluate(List(hd),base)
                fmp1 = new FeatureMerge[Feature,Feature] {
                  var featureName = fmp1.featureName + fmp2.featureName
                  val alg1 = fmp1
                  val alg2 = fmp2
                  val featureSet = fmp1.featureSet ++ fmp2.featureSet
                  val lifter = new Lifter[Feature,Feature] {
                    def lift(x : Feature, y : Feature) = new Feature  {
                      def Val() : List[Activity] = x.behavior
                      def eVal() : Unit =  {
                        for (x <- behavior) x.execute()
                      }
                      override var agent : Agent = null
                      override var behavior : List[Activity] = x.behavior union y.behavior
                    }
                  }
                }

                completed = true

              }
            }

            println("Or operation applied - " + "merged to create " + fmp1.featureName)

          case Xor(_,_) =>
            println("Xor operator")
            var found : Boolean = false
            val fList = h.subFeatures.iterator
            while ((fList.hasNext) && (found==false)) {
              val hd = fList.next()
              if ((resModel(hd.name)==true) && (found==false)) {
                fmp1 = evaluate(List(hd),base)
                found=true
              }
            }
            println("XOr operation applied - " + "merged to create " + fmp1.featureName)


          case Optional(name,_) =>
            println("Optional operator")
            if (resModel(name)==true) {
              val hd = h.subFeatures.head
              fmp1 = evaluate(List(hd),base)
            }
            else fmp1 = baseGenerator
            println("Optional operation applied - " + "to create " + fmp1.featureName)

          case _ =>
            println("Undefined operator")

        }
        println("tail: " + t)


        if (t.isEmpty) {
          fm = fmp1
        }
        else {
          fmp2=evaluateGeneric(t,base)

          fm = new FeatureMerge[Feature,Feature] {
            var featureName = fmp1.featureName + fmp2.featureName
            val alg1 = fmp1
            val alg2 = fmp2
            val featureSet = fmp1.featureSet ++ fmp2.featureSet
            val lifter = new Lifter[Feature,Feature] {
              def lift(x : Feature, y : Feature) = new Feature  {
                def Val() : List[Activity] = x.behavior
                def eVal() : Unit =  {
                  for (x <- behavior) x.execute()
                }
                override var agent : Agent = null
                override var behavior : List[Activity] = x.behavior union y.behavior
              }
            }

          }

          println("merging feature " +  fm.alg1.featureName + " with " + fm.alg2.featureName)
        }

        fm

      case _ =>
        println("Empty feature")
        fm = baseGenerator
        fm
    }

  }


  def evaluate(fMod: List[FOM], base : BaseModelGen): FeatureMerge[Feature,Feature] = {
    println("Model is " + fMod)
    var fm : FeatureMerge[Feature,Feature] = null
    var fmp1 : FeatureMerge[Feature,Feature] = null
    var fmp2 : FeatureMerge[Feature,Feature] = null
    var fmpTemp : FeatureMerge[Feature,Feature] = null
    var f1g : FeatureAlg[Feature] = null
    var f2g : FeatureAlg[Feature] = null

    fMod match {
      case h::t =>
        println("head: " + h)
        h match {
          case And(_,_) =>
            println("And operator")
            val hd = h.subFeatures.head
            val rest = h.subFeatures.tail
            fmp1 = evaluate(List(hd),base)
            for (x <- rest) {
              fmp2 = evaluate(List(x),base)
              //fmp1 = fLMap((fmp1.featureName,fmp2.featureName))
              if (fSMap contains (fmp1.featureSet,fmp2.featureSet))
                fmp1 = fSMap((fmp1.featureSet,fmp2.featureSet))
              else fmp1 = fSMap((fmp2.featureSet,fmp1.featureSet))
            }
            println("And operation applied - " + "merged to create " + fmp1.featureName)

          case Or(_,_) =>
            println("Or operator")
            var found : Boolean = false
            val fList = h.subFeatures.iterator
            while (fList.hasNext) {
              val hd = fList.next()
              if ((resModel(hd.name)==true) && (found==false)) {
                fmp1 = evaluate(List(hd),base)
                found=true
              }
              else
              if ((resModel(hd.name)==true) && (found==true)) {
                fmp2 = evaluate(List(hd),base)
                //fmp1 = fLMap((fmp1.featureName,fmp2.featureName))
                if (fSMap contains (fmp1.featureSet,fmp2.featureSet))
                  fmp1 = fSMap((fmp1.featureSet,fmp2.featureSet))
                else fmp1 = fSMap((fmp2.featureSet,fmp1.featureSet))
              }
            }

            println("Or operation applied - " + "merged to create " + fmp1.featureName)

          case F(name,_) =>
            println("Plain feature")
            f1g = featureMap(name).asInstanceOf[FeatureAlg[Feature]]
            fmp1 = new FeatureMerge[Feature,Feature] {
              var featureName = f1g.featureName
              val alg1 = f1g
              val alg2 = base
              val featureSet = f1g.featureSet
              val lifter = baseLMap(f1g.featureName)
            }

          case Xor(_,_) =>
            println("Xor operator")
            var found : Boolean = false
            val fList = h.subFeatures.iterator
            while ((fList.hasNext) && (found==false)) {
              val hd = fList.next()
              if ((resModel(hd.name)==true) && (found==false)) {
                fmp1 = evaluate(List(hd),base)
                found=true
              }
            }
            println("XOr operation applied - " + "merged to create " + fmp1.featureName)
          case Optional(name,_) =>
            println("Optional operator")
            if (resModel(name)==true) {
              val hd = h.subFeatures.head
              fmp1 = evaluate(List(hd),base)
            }
            else fmp1 = baseGenerator
            println("Optional operation applied - " + "to create " + fmp1.featureName)
          case _ =>
            println("Undefined operator")
        }


        println("tail: " + t)


        if (t.isEmpty) {
          fm = fmp1
        }
        else {
          fmp2=evaluate(t,base)
          //fm = fLMap((fmp1.featureName,fmp2.featureName))
          if (fSMap contains (fmp1.featureSet,fmp2.featureSet))
            fm = fSMap((fmp1.featureSet,fmp2.featureSet))
          else fm = fSMap((fmp2.featureSet,fmp1.featureSet))

          println("merging feature " +  fm.alg1.featureName + " with " + fm.alg2.featureName)
        }

        fm

      case _ =>
        println("Empty feature")
        fm = baseGenerator
        fm
    }
  }




}


class FeatureGraph {
  class FeatureNode (var featureName : String) { self =>
    var f : Feature = _
    var fg : FeatureAlg[Feature] = _
    var fm : FeatureMerge[Feature,Feature] = _
    var connectedLinks: List[LiftLink] = Nil
    def connectTo(node: FeatureNode, liftTo : String) {
      var lifter = new LiftLink(self, node, liftTo)
      connectedLinks = lifter :: connectedLinks
      links = lifter :: links
    }
  }

  class LiftLink(var source : FeatureNode, var target: FeatureNode, var x : String) {
    var liftTo : String = x
  }
  var nodes: List[FeatureNode] = Nil
  var links: List[LiftLink] = Nil

  def newFeatureNode(x: String, ft: Feature) : FeatureNode = {
    val res = new FeatureNode(x) {
      f = ft
    }
    nodes = res :: nodes
    res
  }


}