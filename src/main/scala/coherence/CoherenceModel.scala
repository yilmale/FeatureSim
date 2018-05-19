package coherence


object CoMod {
  var coModel : CoherenceModel = null
  def explain(): Constraint = {
    new Constraint {

    }
  }

  def proposition() : Unit = {
    var p = new Proposition()
    coModel.propositions = p :: coModel.propositions
    println("Inserted new proposition " + coModel.propositions)
  }
}


class CoherenceModel { self =>
  var propositions : List[Proposition] = List()
  var constraints : List[Constraint] = List()
  CoMod.coModel = this

  def subjectTo(c: Constraint*) : CoherenceModel = {
    new CoherenceModel {
      propositions = self.propositions
      constraints = self.constraints ++ c
    }
  }


}

class Proposition {

}

class Constraint {

}

