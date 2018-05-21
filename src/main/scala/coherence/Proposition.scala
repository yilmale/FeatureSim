package coherence

abstract class Proposition {
  var id : String = ""
  var explanation : String = ""
  var activation : Double = 0

  def getNeighbor(c: Constraint): Proposition = {
    c match {
      case e : Explanation => if (e.explanandum.id == this.id) e.explanan else e.explanandum
      case d : Deduction => if (d.consequence.id==this.id) d.premise else d.consequence
      case f: Facilitation => if (f.goal.id==this.id) f.action else f.goal
    }
  }
}

class Data extends Proposition {

}

class Belief extends Proposition {

}

class Goal extends Proposition {

}

class Action extends Proposition {

}
