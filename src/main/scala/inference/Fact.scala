package inference

abstract class Fact {

}

abstract class Clause {
  var wm : WorkingMemory = null
  var truth : Boolean = false
  def evaluate() : Boolean
}

class Assert(f : => Unit, e: => Boolean) extends Clause {
  f
  truth=true

  def evaluate() : Boolean = e
}

object Assert {
  def apply(f: => Unit, e: => Boolean): Clause =  {
    new Assert(f,e)
  }

}


case class belief(percept: Clause)(implicit wm : WorkingMemory) {
  wm.beliefs = this :: wm.beliefs
  percept.wm = wm
}

case class rule(antecedent : List[Clause],consequent : Clause)(implicit rb : RuleBase) {
  rb.rules = this :: rb.rules
}

class RuleBase(wm: WorkingMemory) {
  var rules : List[rule] = List()
  var matchList : List[rule] = null

  def check(r : rule): Boolean = {
    println("Testing rule.....")
    var found: Boolean = true
    var it = r.antecedent.iterator
    while ((it.hasNext) && (found==true)) {
      var a = it.next()
      found = a.evaluate()
    }
    found
  }

  def forwardChain(): Unit = {
    matchList = List()
    rules foreach ( r => {
      if (check(r))
        matchList = r :: matchList
      }
      )
    }
}


class WorkingMemory {
   var beliefs : List[belief] = List()

}

