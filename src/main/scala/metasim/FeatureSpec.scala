package metasim


abstract class FeatureExpression {var fname: String}
case class And(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Or(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class Base(name:String) extends FeatureExpression {var fname = name}
case class Feature(name: String) extends FeatureExpression {var fname = name}
case class Optional(name: String, children: List[FeatureExpression]=null) extends FeatureExpression {var  fname = name}
case class Xor(name:String, children: List[FeatureExpression]) extends FeatureExpression {var fname = name}
case class FeatureTree(node: FeatureExpression,
                       children: List[FeatureExpression] = List[FeatureExpression]())
case class ResolutionModel(rm: scala.collection.mutable.Map[String,Boolean])

object FeatureSpec {
  type RM = scala.collection.mutable.Map[String,Boolean]
  var composite : String = ""
  var featureModel : FeatureTree = null
  var resolution: RM = null
  def apply(fm: FeatureTree,res: ResolutionModel): Array[String] = {
    val n = FeatureTree(Base("F"),List(
      And("F0",List(Feature("F01"),Feature("F02"))),
      Or("F1",List(Feature("F11"),Feature("F12"))),
      Optional("F2", List(Feature("F2"))),
      Feature("F3"),
      Xor("F4",List(Feature("F41"),Feature("F42")))
    ))
    var rmodel = scala.collection.mutable.Map[String,Boolean](
      "F11"->true,
      "F12"->false,
      "F2"-> false,
      "F42"-> true
    )


    featureModel = fm
    resolution = res.rm
    compile()
  }

  def compile(): Array[String] = {
    evaluate(featureModel)
    composite split("_")
  }




  def evaluate(x: FeatureTree): Unit = {

    x.node match {
      case f : Feature => {composite = composite + "_" + f.name}
      case b : Base => {
        composite = composite + b.name
        x.children foreach { c =>
          evaluate(FeatureTree(c))
        }
      }
      case op : Optional => {
        if ((resolution.keys.exists(x => (x == op.fname) && (resolution(op.fname) == true)))) {
          op.children foreach { c =>
            evaluate(FeatureTree(c))
          }
        }
      }
      case a : And => {
        composite = composite + "_" + a.name
        a.children foreach { c =>
           evaluate(FeatureTree(c))
        }
      }
      case o : Or => {
        composite = composite + "_" + o.name
        o.children foreach { c =>
        {
          if ((resolution.keys.exists(x => (x == c.fname) && (resolution(x) == true)))) {
             evaluate(FeatureTree(c))
          }
        }
        }
      }
      case x : Xor => {
        var found = false
        composite = composite + "_" + x.name
        x.children foreach { c =>
          if (found==false) {
            if ((resolution.keys.exists(x => (x == c.fname) && (resolution(x) == true)))) {
              found = true
              evaluate(FeatureTree(c))
            }
          }
        }
      }
    }
  }
}