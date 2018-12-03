package coherence



object CoMod {
  var coModel : CoherenceModel = null
  type DATA = (String, String, Double)

  def constraintGen(e: Constraint,exp: String, target: String): List[Constraint] = {
    coModel.CG(coModel.P(target)) = e :: coModel.CG(coModel.P(target))
    coModel.CG(coModel.P(exp)) = e :: coModel.CG(coModel.P(exp))
    coModel.C = e :: coModel.C
    List(e)
  }

  def composePairs(L: List[String]): List[(String,String)] = {
    def compose(f: String, R: List[String]): List[(String,String)] = {
      val PL = for (x <- R) yield (f,x)
      PL
    }

    var S : List[(String,String)] = List()
    var T = L
    if (T.length >= 2) {
      while (T.tail.isEmpty == false) {
        val a = T.head
        S = S ++ compose(a,T.tail)
        T = T.tail
      }
    }
    S
  }



  def explain(exp: String, target: String, w: Double=0.05): List[Constraint] = {
    val e = new Explanation {
      explanandum = coModel.P(target)
      explanan = coModel.P(exp)
      weight = w
    }
   constraintGen(e,exp,target)
  }


  def contradict(exp: String, target: String, w: Double=(-0.2)): List[Constraint] = {
    val e = new Explanation {
      explanandum = coModel.P(target)
      explanan = coModel.P(exp)
      weight = w
    }
    constraintGen(e,exp,target)
  }

  def incompatible(exp: String, target: String, w: Double=(-0.2)): List[Constraint] = {
    val e = new Deduction {
      consequence = coModel.P(target)
      premise = coModel.P(exp)
      weight = w
    }
    constraintGen(e,exp,target)
  }

  def conflict(act1: String, act2: String, w: Double=(-0.2)): List[Constraint] = {
    val e = new Facilitation {
      goal = coModel.P(act2)
      action = coModel.P(act1)
      weight = w
    }
    constraintGen(e,act1,act2)
  }


  def explain(exp: List[String], target: String, w: Double): List[Constraint] = {

    var cl : List[Constraint] = List()

    for (s <- exp) {
      val e = new Explanation {
        explanandum = coModel.P(target)
        explanan = coModel.P(s)
        weight = w/exp.length
      }
      coModel.CG(coModel.P(target)) = e :: coModel.CG(coModel.P(target))
      coModel.CG(coModel.P(s)) = e :: coModel.CG(coModel.P(s))
      coModel.C = e :: coModel.C
      cl = e :: cl
    }

    var pairs = composePairs(exp)
    for (p <- pairs) {
      val e = new Explanation {
        explanandum = coModel.P(p._1)
        explanan = coModel.P(p._2)
        weight = w/exp.length
      }
      coModel.CG(coModel.P(p._1)) = e :: coModel.CG(coModel.P(p._1))
      coModel.CG(coModel.P(p._2)) = e :: coModel.CG(coModel.P(p._2))
      cl = e :: cl
    }
    cl
  }

  def deduce(exp: String, target: String, w: Double=0.05): List[Constraint] = {
    val e = new Deduction {
      consequence = coModel.P(target)
      premise = coModel.P(exp)
      weight = w
    }

    constraintGen(e,exp,target)
  }

  def deduce(exp: List[String], target: String, w: Double): List[Constraint] = {

    var cl : List[Constraint] = List()

    for (s <- exp) {
      val e = new Deduction {
        consequence = coModel.P(target)
        premise = coModel.P(s)
        weight = w/exp.length
      }
      coModel.CG(coModel.P(target)) = e :: coModel.CG(coModel.P(target))
      coModel.CG(coModel.P(s)) = e :: coModel.CG(coModel.P(s))
      coModel.C = e :: coModel.C
      cl = e :: cl
    }

    var pairs = composePairs(exp)
    for (p <- pairs) {
      val e = new Deduction {
        consequence = coModel.P(p._1)
        premise = coModel.P(p._2)
        weight = w/exp.length
      }
      coModel.CG(coModel.P(p._1)) = e :: coModel.CG(coModel.P(p._1))
      coModel.CG(coModel.P(p._2)) = e :: coModel.CG(coModel.P(p._2))
      cl = e :: cl
    }
    cl
  }

  def facilitate(a: String, g: String, w: Double=0.05): List[Constraint] = {
    val e = new Facilitation {
      goal = coModel.P(g)
      action = coModel.P(a)
      weight = w
    }

    constraintGen(e,a,g)
  }

  def facilitate(a: List[String], g: String, w: Double): List[Constraint] = {

    var cl : List[Constraint] = List()

    for (s <- a) {
      val e = new Facilitation {
        goal = coModel.P(g)
        action = coModel.P(s)
        weight = w/a.length
      }
      coModel.CG(coModel.P(g)) = e :: coModel.CG(coModel.P(g))
      coModel.CG(coModel.P(s)) = e :: coModel.CG(coModel.P(s))
      coModel.C = e :: coModel.C
      cl = e :: cl
    }

    var pairs = composePairs(a)
    for (p <- pairs) {
      val e = new Facilitation {
        goal = coModel.P(p._1)
        action = coModel.P(p._2)
        weight = w/a.length
      }
      coModel.CG(coModel.P(p._1)) = e :: coModel.CG(coModel.P(p._1))
      coModel.CG(coModel.P(p._2)) = e :: coModel.CG(coModel.P(p._2))
      cl = e :: cl
    }
    cl
  }

  def data(data: DATA*): Unit = {

    for (d <- data) {
      coModel.P += (d._1 -> new Data(d._1,d._2,d._3))
      coModel.CG += (coModel.P(d._1) -> List[Constraint]())
    }
  }

  def context(ident: String, expl: String, act: Double=0.01): Unit = {
    coModel.P += (ident -> new Context(ident,expl,act))
    coModel.CG += (coModel.P(ident) -> List[Constraint]())
  }

  def belief(ident: String, expl: String, act: Double=0.01): Unit = {
    coModel.P += (ident -> new Belief(ident,expl,act))
    coModel.CG += (coModel.P(ident) -> List[Constraint]())
  }

  def feature(ident: String, expl: String, act: Double=0.01): Unit = {
    coModel.P += (ident -> new Feature(ident,expl,act))
    coModel.CG += (coModel.P(ident) -> List[Constraint]())
  }

  def goal(ident: String, expl: String, act: Double=0.01): Unit = {
    coModel.P += (ident -> new Goal(ident,expl,act))
    coModel.CG += (coModel.P(ident) -> List[Constraint]())
  }

  def activity(ident: String, expl: String, act: Double=0.01): Unit = {
    coModel.P += (ident -> new Action(ident,expl,act))
    coModel.CG += (coModel.P(ident) -> List[Constraint]())
  }

}

object FeatureConstraintModel {
  def apply(block: => Unit): CoherenceModel = {
    new CoherenceModel {
      block
    }
  }
}


class CoherenceModel { self =>
  type Resolution = collection.mutable.Map[Proposition,List[Proposition]]
  var P : scala.collection.mutable.Map[String,Proposition] = scala.collection.mutable.Map()
  var C : List[Constraint] = List()
  var CG : scala.collection.mutable.Map[Proposition,List[Constraint]] = scala.collection.mutable.Map()
  val decayRate : Double = 0.05
  val MAX : Double = 1.0
  val MIN : Double = -1.0
  CoMod.coModel = this

  def subjectTo(c: List[Constraint]*) : CoherenceModel = {
    new CoherenceModel {
      P = self.P
      C = self.C
      CG = self.CG
    }
  }

  def getActiveNodes(threshold : Double) : List[Proposition] = {
    var activated =List[Proposition]()
    for (p <- P values) {
      if (p.activation >= threshold) activated = p :: activated
    }
    activated
  }

  def evaluate(): CoherenceModel = {
    var activations= scala.collection.mutable.Map[Proposition,Double]()
    var netFlow = scala.collection.mutable.Map[Proposition,Double]()
    for (p <- P values) {
      activations += (p -> p.activation)
      netFlow += (p->0)
    }
    val threshold : Double = 0.01
    var maxDiff: Double = 0
    val maxIteration = 200
    var iteration : Int = 0
    do  {
      for (p <- P values) {
        var flow : Double = 0
        netFlow(p) = {
          for (c <- CG(p)) {
            val n = p.getNeighbor(c)
            flow=flow+(c.weight*activations(n))
          }
          flow
        }

        if (flow > 0)
          p.activation = (activations(p)*(1-decayRate)) + (flow*(MAX-activations(p)))
        else
          p.activation  = (activations(p)*(1-decayRate)) + (flow*(activations(p)-MIN))
        maxDiff=math.max(maxDiff,math.abs(p.activation-activations(p)))
      }

      for (p <- P values) {
        activations(p) = p.activation
      }
      iteration = iteration+1
    } while ((maxDiff > threshold) && (iteration <= maxIteration))
    this
  }

  def evaluate_Asynch() {
    val threshold : Double = 0.01
    var maxDiff: Double = 0
    val maxIteration = 200
    var iteration : Int = 0

    var netFlow = scala.collection.mutable.Map[Proposition,Double]()
    for (p <- P values) {
      netFlow += (p->0)
    }
    do  {
      for (p <- P values) {
        var flow : Double = 0
        netFlow(p) = {
          for (c <- CG(p)) {
            val n = p.getNeighbor(c)
            flow=flow+(c.weight*n.activation)
          }
          flow
        }
        val oldActivation : Double = p.activation
        if (flow > 0)
          p.activation = (p.activation*(1-decayRate)) + (flow*(MAX-p.activation))
        else
          p.activation  = (p.activation*(1-decayRate)) + (flow*(p.activation-MIN))
        maxDiff=math.max(maxDiff,math.abs(p.activation-oldActivation))
      }

      iteration = iteration+1
    } while ((maxDiff > threshold) && (iteration <= maxIteration))
  }

  override def toString: String = {
    var str = s""
    for (p <- P) {
      str =str + s"Proposition: ${p._1}\nAdjacency list: \n"

      for (x <- CG(p._2)) {
        x match {
          case e : Explanation => str = str + s"${e.explanandum.id} --- ${e.explanan.id}  weight: ${e.weight} \n"
          case d: Deduction => str = str + s"${d.premise.id} --- ${d.consequence.id}  weight: ${d.weight} \n"
          case f: Facilitation => str = str + s"${f.goal.id} --- ${f.action.id}  weight: ${f.weight} \n"
        }
      }
    }
    str = str + s"Activations: \n"

    for (p <- P values)
      str = str + s"${p.id}: ${p.activation} \n"

    str
  }

  def generateResolution() : Resolution = {
    var RM = collection.mutable.Map[Proposition,List[Proposition]]()
    for (p <- P values) {
      p match {
        case p : Feature => {
          if (p.activation > 0)
            RM += (p -> List[Proposition]())
        }
        case _ =>
      }
    }
    var m = RM.keys.map(_.id)
    for (e <- C) {
      e match {
        case e : Facilitation => {
          if (m.exists(_ == e.action.id) && (e.weight > 0)) {
            e.goal match {
              case x : Feature if (e.goal.activation > 0)
                        => RM (e.action) = x :: RM (e.action)
              case _  =>
            }
          }
        }
        case _ =>
      }
    }
    RM
  }

  def serialize(resModel: Resolution) : Array[String] = {
    var featureList = List[String]()
    object VColor extends Enumeration {
      type VColor = Value
      val WHITE, GRAY, BLACK = Value
    }
    import VColor._
    var vertexColor = collection.mutable.Map[Proposition,VColor]()
    for (k <- resModel keys) {
      vertexColor += (k -> WHITE)
    }

    for (n <- resModel keys) {
      if (vertexColor(n)==WHITE)
        visit(n)
    }

    def visit(v: Proposition): Unit = {
      vertexColor(v)=GRAY
      for (n <- resModel(v)) {
        if (vertexColor(n)==WHITE) visit(n)
      }
      vertexColor(v)=BLACK
      featureList = v.id :: featureList
    }
    featureList.reverse.toArray
  }

}


