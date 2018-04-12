package inference

abstract class Fact {
  var name : String
}


case class Belief(var fact: Fact)

case class Rule(var antecedent : Fact*)(var consequent : Fact)


abstract class BeliefBase {
  var beliefs : List[Belief]
}

