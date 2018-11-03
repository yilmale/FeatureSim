package coherence

abstract class Proposition {
  var id : String
  var explanation : String
  var activation : Double

  def getNeighbor(c: Constraint): Proposition = {
    c match {
      case e : Explanation => if (e.explanandum.id == this.id) e.explanan else e.explanandum
      case d : Deduction => if (d.consequence.id==this.id) d.premise else d.consequence
      case f: Facilitation => if (f.goal.id==this.id) f.action else f.goal
    }
  }
}

class Data(dataId: String, expl: String, act: Double) extends Proposition {
var id = dataId
var explanation = expl
var activation = act
}

class Feature(fid: String, expl: String, act: Double) extends Proposition {
  var id = fid
  var explanation = expl
  var activation = act
}

class Belief(bid: String, expl: String, act: Double) extends Proposition {
  var id = bid
  var explanation = expl
  var activation = act
}

class Context(cid: String, expl: String, act: Double) extends Proposition {
  var id = cid
  var explanation = expl
  var activation = act
}

class Goal(gid: String, expl: String, act: Double) extends Proposition {
  var id = gid
  var explanation = expl
  var activation = act
}

class Action(aid: String, expl: String, act: Double) extends Proposition {
  var id = aid
  var explanation = expl
  var activation = act
}
