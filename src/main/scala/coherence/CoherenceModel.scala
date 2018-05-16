package coherence


object CoMod {
  var coModel : CoherenceModel = null

  def explain(target: String, exp: String, w: Double): Constraint = {
    val e = new Explanation {
      explanandum = coModel.P(target)
      explanan = coModel.P(exp)
      weight = w
    }

    coModel.CG(coModel.P(target)) = e :: coModel.CG(coModel.P(target))
    coModel.CG(coModel.P(exp)) = e :: coModel.CG(coModel.P(exp))
    e
  }

  def data(ident: String, expl: String, confidence: Double): Unit = {
    coModel.P += (ident -> new Belief() {
      id = ident
      explanation = expl
      activation = confidence
    })
    coModel.CG += (coModel.P(ident) -> List[Constraint]())
  }

  def belief(ident: String, expl: String, act: Double): Unit = {
    coModel.P += (ident -> new Belief() {
      id = ident
      explanation = expl
      activation = act
    })
    coModel.CG += (coModel.P(ident) -> List[Constraint]())
  }

  def goal(ident: String, expl: String, act: Double): Unit = {
    coModel.P += (ident -> new Goal() {
      id = ident
      explanation = expl
      activation = act
    })
    coModel.CG += (coModel.P(ident) -> List[Constraint]())
  }

}


class CoherenceModel { self =>
  var P : scala.collection.mutable.Map[String,Proposition] = scala.collection.mutable.Map()
  var C : List[Constraint] = List()
  var CG : scala.collection.mutable.Map[Proposition,List[Constraint]] = scala.collection.mutable.Map()
  CoMod.coModel = this

  def subjectTo(c: Constraint*) : CoherenceModel = {
    new CoherenceModel {
      P = self.P
      C = self.C ++ c
      CG = self.CG
    }
  }


}


