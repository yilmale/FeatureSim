package coherence

abstract class Constraint {
   var weight : Double = 0
}

class Explanation extends Constraint {
  var explanandum : Proposition = _
  var explanan : Proposition = _
}
