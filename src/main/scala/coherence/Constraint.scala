package coherence

abstract class Constraint {
   var weight : Double = 0
}

class Explanation extends Constraint {
  var explanandum : Proposition = _
  var explanan : Proposition = _
}

class Deduction extends Constraint {
  var premise: Proposition = _
  var consequence: Proposition = _
}

class Facilitation extends Constraint {
  var goal: Proposition = _
  var action: Proposition = _
}
