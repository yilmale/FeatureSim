package coherence

abstract class Proposition {
  var id : String = ""
  var explanation : String = ""
  var activation : Double = 0

}

class Data extends Proposition {

}

class Belief extends Proposition {

}

class Goal extends Proposition {

}

class Action extends Proposition {

}
