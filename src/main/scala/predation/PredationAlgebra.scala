package predation

import agent._
import featuremodel._
import simulation._

import scala.xml._

/**
  * Created by yilmaz on 2/7/17.
  */
trait PredationFeature extends Feature {
  def Val(): List[Activity] = {
    behavior
  }

  def eVal(): Unit = {
    for (x <- behavior) x.execute()
  }
}

trait PreyMove extends PredationFeature {
  val featureName = "PreyMove"

}

trait PreyDeath extends PredationFeature {
  val featureName = "PreyDeath"

}


trait PreyPlainReproduction extends PredationFeature {
  val featureName = "PreyPlainReproduction"

}

trait PreyFoliageReproduction extends PredationFeature {
  val featureName = "PreyFoliageReproduction"

}


trait PreyMoveGenerator extends FeatureAlg[Feature] {self =>
  var featureName = "PreyMove"
  val featureSet = Set("PreyMove")
  def Def(x: List[Activity]): Feature = new PreyMove {
    override var agent : Agent = _
    override var behavior = x

  }

  def Add(e1: Feature, e2: Feature): Feature =  new PreyMove {
    override var agent : Agent = _
    override var behavior : List[Activity]  = e1.Val() ::: e2.Val()
  }

  def Cons() : Feature = new PreyMove {
    override var agent : Agent = _
    var behavior : List[Activity] = List()

  }

  def Cons(a : Agent) : Feature = new PreyMove {self =>
    override var agent : Agent = a
    agent.agentType = "Prey"

    PreyModel.baseBehavior.Cons(a)

    object PreyRG extends ActivityGenerator

    val move = PreyRG.Gen((agent.state.\("caught").text)!="true") {
      var g = ModelUtility.gridView
      var cX = g.location(this.agent)._1
      var cY = g.location(this.agent)._2
      var r = scala.util.Random
      var direction: Double = r.nextDouble()
      var bound : Int = 2
      var dx = r.nextInt(bound)
      if (direction < 0.5) dx = -dx
      var dy = r.nextInt(bound)
      direction = r.nextDouble()
      if (direction < 0.5) dy = -dy
      cX = math.abs(cX + dx) % g.getXDim
      cY = math.abs(cY + dy) % g.getYDim
      g.move(this.agent, cX, cY)
    }(false)

    override var behavior : List[Activity] = List(move)
    a.addActivity(move)
  }
}


trait PreyDeathGenerator extends FeatureAlg[Feature] {
  var featureName = "PreyDeath"
  val featureSet = Set("PreyDeath")
  def Def(x: List[Activity]): Feature = new PreyDeath {
    override var agent : Agent = _
    override var behavior = x
  }

  def Add(e1: Feature, e2: Feature): Feature =  new PreyDeath {
    override var agent : Agent = _
    override var behavior : List[Activity]  = e1.Val() ::: e2.Val()
  }

  def Cons() : Feature = new PreyDeath {
    override var agent : Agent = _
    override var behavior : List[Activity] = List()

  }

  def Cons(a : Agent) : Feature = new PreyDeath {
    override var agent : Agent = a
    agent.agentType = "Prey"
    PreyModel.baseBehavior.Cons(a)
    object PreyRG extends ActivityGenerator

    val death = PreyRG.Gen((agent.state.\("caught").text)!="false") {
      println("This prey was caught...........................................")
      ModelUtility.remove(this.agent)
    }(false)

    override var behavior : List[Activity] = List(death)
    a.addActivity(death)
  }
}


trait PreyPlainReproductionGenerator extends FeatureAlg[Feature] {
  var featureName = "PreyPlainReproduction"
  val featureSet = Set("PreyPlainReproduction")
  def Def(x: List[Activity]): Feature = new PreyPlainReproduction {
    override var agent : Agent = _
    override var behavior = x
  }

  def Add(e1: Feature, e2: Feature): Feature =  new PreyPlainReproduction {
    override var agent : Agent = _
    override var behavior : List[Activity]  = e1.Val() ::: e2.Val()
  }

  def Cons() : Feature = new PreyPlainReproduction {

    override var agent : Agent = _
    var behavior : List[Activity] = List()
  }

  def Cons(a : Agent) : Feature = new PreyPlainReproduction {
    override var agent : Agent = a
    agent.agentType = "Prey"
    PreyModel.baseBehavior.Cons(a)
    object PreyRG extends ActivityGenerator
    object ag extends AgentGenerator
    val preyGen = FOModel(PreyModel)
    val reproduction = PreyRG.Gen((agent.state.\("caught").text)!="true") {
      var g = ModelUtility.gridView
      var r = scala.util.Random
      var dP = r.nextDouble()
      if (dP <= ModelUtility.experiment.params("reproductionRate").asInstanceOf[Double]) {
        var cX = g.location(this.agent)._1
        var cY = g.location(this.agent)._2
        val prey = ag.Cons(PreyModel.stateModel)
        preyGen.Cons(prey)
        g.move(prey, cX, cY)
        //ModelUtility.ctx.add(new Prey(), cX, cY)
      }
    }(false)
    var behavior : List[Activity] = List(reproduction)
    a.addActivity(reproduction)
  }
}

trait PreyFoliageReproductionGenerator extends FeatureAlg[Feature] {
  var featureName = "PreyFoliageReproduction"
  val featureSet = Set("PreyFoliageReproduction")
  def Def(x: List[Activity]): Feature = new PreyFoliageReproduction {
    override var agent : Agent = _
    override var behavior = x
  }

  def Add(e1: Feature, e2: Feature): Feature =  new PreyFoliageReproduction {
    override var agent : Agent = _
    override var behavior : List[Activity]  = e1.Val() ::: e2.Val()
  }

  def Cons() : Feature = new PreyFoliageReproduction {
    override var agent : Agent = _
    var behavior : List[Activity] = List()
  }

  def Cons(a : Agent) : Feature = new PreyFoliageReproduction {
    override var agent : Agent = a
    agent.agentType = "Prey"
    PreyModel.baseBehavior.Cons(a)
    object PreyRG extends ActivityGenerator
    object ag extends AgentGenerator
    val preyGen = FOModel(PreyModel)
    val reproduction = PreyRG.Gen((agent.state.\("caught").text)!="true") {
      var foliageRate : Double = ModelUtility.experiment.params("foliageRate").asInstanceOf[Double]
      var g = ModelUtility.gridView
      var r = scala.util.Random
      var fr = r.nextDouble()
      if (fr <= foliageRate) {
        var dP = r.nextDouble()
        if (dP <= ModelUtility.experiment.params("reproductionRate").asInstanceOf[Double]) {
          var cX = g.location(this.agent)._1
          var cY = g.location(this.agent)._2
          val prey = ag.Cons(PreyModel.stateModel)
          preyGen.Cons(prey)
          g.move(prey, cX, cY)
          //ModelUtility.ctx.add(new Prey(), cX, cY)
        }
      }
    }(false)

    var behavior : List[Activity] = List(reproduction)
    a.addActivity(reproduction)
  }
}


object LiftFPreyMove_Base extends Lifter[Feature,Feature] {
  def lift(x : Feature, y : Feature) = new PreyMove  {
    override var agent : Agent = null
    override var behavior : List[Activity] = x.behavior
  }
}


object LiftPreyDeath_Base extends Lifter[Feature,Feature] {
  def lift(x : Feature, y : Feature) = new PreyDeath {
    override var agent : Agent = _
    var behavior: List[Activity] = x.behavior
  }
}

object LiftPreyPlainReproduction_Base extends Lifter[Feature,Feature] {
  def lift(x : Feature, y : Feature) = new PreyPlainReproduction {
    override var agent : Agent = null
    override var behavior : List[Activity] = x.behavior
  }
}


object LiftPreyFoliageReproduction_Base extends Lifter[Feature,Feature] {
  def lift(x : Feature, y : Feature) = new PreyPlainReproduction {
    override var agent : Agent = null
    override var behavior : List[Activity] = x.behavior
  }
}


object pmg extends PreyMoveGenerator
object pdg extends PreyDeathGenerator
object pfrg extends PreyFoliageReproductionGenerator
object pprg extends PreyPlainReproductionGenerator


object LiftPreyMove_PreyDeath extends Lifter[Feature,Feature] {
  def lift(x : Feature, y : Feature) = new PreyMove with PreyDeath {
    override val featureName = "PreyMove_PreyDeath"
    var agent : Agent = null
    var behavior : List[Activity] = x.behavior union y.behavior
  }
}

object LiftPreyMove_PreyDeath_PreyPlainReproduction extends Lifter[Feature,Feature] {
  def lift(x : Feature, y : Feature) = new PreyMove with PreyDeath with PreyPlainReproduction {
    override val featureName : String = "PreyMove_PreyDeath_PreyPlainReproduction"
    var agent : Agent = null
    var behavior : List[Activity] = x.behavior union y.behavior
  }
}

object LiftPreyMove_PreyDeath_PreyFoliageReproduction extends Lifter[Feature,Feature] {
  def lift(x : Feature, y : Feature) = new PreyMove with PreyDeath with PreyFoliageReproduction {
    override val featureName = "PreyMove_PreyDeath_PreyFoliageReproduction"
    var agent : Agent = null
    var behavior : List[Activity] = x.behavior union y.behavior
  }
}

object pm_pd extends FeatureMerge[Feature,Feature] {
  var featureName = "PreyMove_PreyDeath"
  val alg1 = pmg
  val alg2 = pdg
  val featureSet = pmg.featureSet union pdg.featureSet
  val lifter = LiftPreyMove_PreyDeath
}

object pmd_pr extends FeatureMerge[Feature,Feature] {
  var featureName = "PreyMove_PreyDeath_PreyPlainReproduction"
  val alg1 = pm_pd
  val alg2 = pprg
  val featureSet = pm_pd.featureSet union pprg.featureSet
  val lifter = LiftPreyMove_PreyDeath_PreyPlainReproduction
}

object pmd_fr extends FeatureMerge[Feature,Feature] {
  var featureName = "PreyMove_PreyDeath_PreyFoliageReproduction"
  val alg1 = pm_pd
  val alg2 = pfrg
  val featureSet = pm_pd.featureSet union pfrg.featureSet
  val lifter = LiftPreyMove_PreyDeath_PreyFoliageReproduction
}


object PreyModel extends VariabilityModel {

  val agentType : String = "Prey"
  val captured : Boolean = false
  var stateModel = <state>
    <agentType>{agentType}</agentType>
    <caught>{captured}</caught>
    /</state>

  val featureModel = FModel("Prey",
    List(
      And("PMD",
        List(
          F("PreyMove"),
          F("PreyDeath"))
      ),
      Xor("PreyReproduction", List(
        F("PreyFoliageReproduction"),
        F("PreyPlainReproduction")
      ))
    )
  )


  val features = Map[String, Object]("PreyMove" -> pmg, "PreyDeath" -> pdg, "PreyPlainReproduction" -> pprg,
    "PreyFoliageReproduction" -> pfrg)


  val resolution = Map[String, Boolean]("PreyMove"->true, "PreyDeath"->true, "PreyPlainReproduction" -> false,
    "PreyFoliageReproduction" -> true)

  val  composer = Map[(String,String),FeatureMerge[Feature,Feature]](
    ("PreyMove","PreyDeath")->pm_pd, ("PreyMove_PreyDeath","PreyPlainReproduction")->pmd_pr,
    ("PreyPlainReproduction","PreyMove_PreyDeath")->pmd_pr, ("PreyMove_PreyDeath","PreyPFoliageReproduction")->pmd_fr,
    ("PreyPFoliageReproduction","PreyMove_PreyDeath")->pmd_fr
  )

  val composer1 = Map[(Set[String],Set[String]),FeatureMerge[Feature,Feature]](
    (Set("PreyMove"),Set("PreyDeath"))->pm_pd,
    (Set("PreyDeath","PreyMove"),Set("PreyPlainReproduction"))->pmd_pr,
    (Set("PreyPlainReproduction"),Set("PreyDeath","PreyMove"))->pmd_pr,
    (Set("PreyDeath","PreyMove"),Set("PreyFoliageReproduction"))->pmd_fr,
    (Set("PreyFoliageReproduction"),Set("PreyDeath","PreyMove"))->pmd_fr
  )

  val base = Map[String,Lifter[Feature,Feature]]("PreyMove"->LiftFPreyMove_Base, "PreyDeath"->LiftPreyDeath_Base,
    "PreyPlainReproduction"->LiftPreyPlainReproduction_Base,"PreyFoliageReproduction"->LiftPreyFoliageReproduction_Base)


  val baseBehavior = new BaseModelGen {

    override def Cons(a : Agent): Feature = new BaseModel {
      var agent : Agent = a
      var featureName = "base"
      var behavior: List[Activity] = null
      println("construct base")
      a.dataModel = new State {
        var description = "Prey Data Model"
        var caught : Boolean = false
      }

      var sm = agent.state
      //println("State model for " + agent.agentType +  " is " + sm)
      //println("The caught field is " +  sm.\("caught").text)


      if (a.messageProcessor == null) {
        object PreyRG extends ActivityGenerator
        val processor = PreyRG.Gen(true) {
          var m: scala.xml.Node = null
          var msg : String = ""
          m = a.receive()
          while (m != null) {
            m match {
              case <caught>{contents}</caught> => {
                if (contents.toString == "true") {
                  val newDescription : String = agent.state.\("agentType").text
                  val newCaptured : Boolean = true
                  agent.state = <state>
                    <agentType>{newDescription}</agentType>
                    <caught>{newCaptured}</caught>
                    /</state>
                }
              }
            }
            m = a.receive()
          }
        }(false)
        a.messageProcessor = processor
      }


      def Val(): List[Activity] = {
        behavior
      }

      def eVal(): Unit = {

      }
    }

  }

}