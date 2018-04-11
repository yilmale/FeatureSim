package inference

abstract class Fact {
  var name : String
}

abstract class Rule {
  var antecedent : List[Fact]
  var consequent : Fact
}


