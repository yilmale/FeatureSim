package metasim

abstract class FeatureExpression {var fname: String}
case class And(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Or(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Base(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Feature(name: String) extends FeatureExpression {var fname = name}
case class Optional(name: String, children: List[FeatureExpression]=null) extends FeatureExpression {var  fname = name}
case class Xor(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class FeatureTree(node: FeatureExpression)

object FeatureSpec {
  def apply(): Unit = {
    val n = FeatureTree(Base("F",List(
      And("F0",List(Feature("F01"),Feature("F02"))),
      Or("F1",List(Feature("F11"),Feature("F12"))),
      Optional("F2", List(Feature("F2"))),
      Feature("F3"),
      Xor("F4",List(Feature("F41"),Feature("F42")))
    )))
    var rmodel = scala.collection.mutable.Map[String,Boolean](
      "F11"->true,
      "F12"->false,
      "F2"-> false,
      "F42"-> true
    )

    val m = FeatureTree(Base("base", List(
      Feature("featureb"),
      Feature("featurec")
    )))

    var rmodel1 = scala.collection.mutable.Map[String,Boolean](
      "featureb" -> true,
      "featureb" -> true
    )


    val c  = evaluate(m,rmodel1)
    println(c)
  }


  def evaluate(x: FeatureTree, resMod: scala.collection.mutable.Map[String, Boolean]): String = {
    var composite : String = ""
    x.node match {
      case f : Feature => {composite = composite + "_" + f.name; composite}
      case b : Base => {
        composite = composite + b.name
        b.children foreach { c =>
          composite = composite + evaluate(FeatureTree(c),resMod)
        }
        composite
      }
      case op : Optional => {
        if ((resMod.keys.exists(x => (x == op.fname) && (resMod(op.fname) == true)))) {
          op.children foreach { c =>
            composite = composite + "_" + evaluate(FeatureTree(c),resMod)
          }
        }
        composite
      }
      case a : And => {
        composite = composite + "_" + a.name
        a.children foreach { c =>
          composite = composite + "_" + evaluate(FeatureTree(c),resMod)
        }
        composite
      }
      case o : Or => {
        composite = composite + "_" + o.name
        o.children foreach { c =>
        {
          if ((resMod.keys.exists(x => (x == c.fname) && (resMod(x) == true)))) {
            composite = composite + "_" + evaluate(FeatureTree(c),resMod)
          }
        }
        }
        composite
      }
      case x : Xor => {
        var found = false
        composite = composite + "_" + x.name
        x.children foreach { c =>
          if (found==false) {
            if ((resMod.keys.exists(x => (x == c.fname) && (resMod(x) == true)))) {
              found = true
              composite = composite + "_" + evaluate(FeatureTree(c),resMod)
            }
          }
        }
        composite
      }
    }
  }
}