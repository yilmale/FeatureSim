package inference



abstract class Clause {
  var wm : WorkingMemory=null
  var truth : Boolean = false
  def evaluate() : Boolean = {
    println("Evaluating " + this)
    var found = false

    var it=wm.beliefs.iterator
    while ((it.hasNext) && (found==false)) {
      if (it.next().percept == this) found=true
    }
    found
  }
}

class Assert(f : => Unit, e: => Boolean) extends Clause {
  f
  truth=true

  override def evaluate() : Boolean = e
}

object Assert {
  def apply(f: => Unit, e: => Boolean): Clause =  {
    new Assert(f,e)
  }

}


case class belief(percept: Clause,truth : Boolean = true)(implicit wm : WorkingMemory) {
  wm.beliefs = this :: wm.beliefs
  percept.wm = wm
}

case class rule(antecedent : List[Clause],consequent : Clause)(implicit rb : RuleBase) {
  rb.rules = this :: rb.rules
  antecedent foreach (a => {a.wm=rb.wm})
  consequent.wm=rb.wm
}

class RuleBase(wmi: WorkingMemory) {
  var wm : WorkingMemory = wmi
  var rules : List[rule] = List()
  var matchList : List[rule] = null

  def check(r : rule): Boolean = {
    println("Testing rule....." + r)
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
    println("Match list is " + matchList)
    }
}


class WorkingMemory {
   var beliefs : List[belief] = List()

}

