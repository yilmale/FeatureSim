package inference

case class eats(name: String, fruit: String) extends Clause
case class fruit(name: String) extends Clause

object MyWorkingMemory extends WorkingMemory {
  var a : Int = 3
}



class InferenceTest {
   import MyWorkingMemory._
   implicit var wm = MyWorkingMemory
   implicit var rb = new RuleBase(wm)

   var x=belief(eats("levent","apple"))
   belief(eats("joe","orange"))

   rule(List(eats("levent","orange")),(fruit("apple")))

   println("old value is " + a)
   belief(Assert({a=4},(a==4)))
   println("new value is " + a)
   println(wm.beliefs)
   println(rb.rules)

   var y = rb.rules.head.antecedent.head

   println("The rule clause is " + y)
   println("belief is " + x.percept)

   if (x.percept == y) println("same") else println("different")

   rb.forwardChain()

}
