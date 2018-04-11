package inference

case class eats(name: String, fruit: String) extends Fact
case class fruit(name: String) extends Fact

class InferenceTest {
   var x = Belief(eats("levent","apple"))
   var y = Rule(eats("levent","apple"))(fruit("apple"))

}
