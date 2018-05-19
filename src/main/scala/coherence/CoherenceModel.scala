package coherence


object CoMod {
  var coModel : CoherenceModel = null
  type DATA = (String, String, Double)


  def explain(exp: String, target: String, w: Double): List[Constraint] = {
    val e = new Explanation {
      explanandum = coModel.P(target)
      explanan = coModel.P(exp)
      weight = w
    }

    coModel.CG(coModel.P(target)) = e :: coModel.CG(coModel.P(target))
    coModel.CG(coModel.P(exp)) = e :: coModel.CG(coModel.P(exp))
    List(e)
  }


  def contradict(exp: String, target: String, w: Double): List[Constraint] = {
    val e = new Explanation {
      explanandum = coModel.P(target)
      explanan = coModel.P(exp)
      weight = -w
    }

    coModel.CG(coModel.P(target)) = e :: coModel.CG(coModel.P(target))
    coModel.CG(coModel.P(exp)) = e :: coModel.CG(coModel.P(exp))
    List(e)
  }


  def explain(exp: List[String], target: String, w: Double): List[Constraint] = {
    def composePairs(L: List[String]): List[(String,String)] = {
      def compose(f: String, R: List[String]): List[(String,String)] = {
        val PL = for (x <- R) yield (f,x)
        PL
      }

      var S : List[(String,String)] = List()
      var T = L
      if (T.length >= 2) {
        while (T.tail.isEmpty == false) {
          val a = T.head
          S = S ++ compose(a,T.tail)
          T = T.tail
        }
      }
      S
    }

    var cl : List[Constraint] = List()

    for (s <- exp) {
      val e = new Explanation {
        explanandum = coModel.P(target)
        explanan = coModel.P(s)
        weight = w/exp.length
      }
      coModel.CG(coModel.P(target)) = e :: coModel.CG(coModel.P(target))
      coModel.CG(coModel.P(s)) = e :: coModel.CG(coModel.P(s))
      cl = e :: cl
    }

    var pairs = composePairs(exp)
    for (p <- pairs) {
      val e = new Explanation {
        explanandum = coModel.P(p._1)
        explanan = coModel.P(p._2)
        weight = w/exp.length
      }
      coModel.CG(coModel.P(p._1)) = e :: coModel.CG(coModel.P(p._1))
      coModel.CG(coModel.P(p._2)) = e :: coModel.CG(coModel.P(p._2))
      cl = e :: cl
    }
    cl
  }

  def data(data: DATA*): Unit = {

    for (d <- data) {
      coModel.P += (d._1 -> new Data() {
        id = d._1
        explanation = d._2
        activation = d._3
      })
      coModel.CG += (coModel.P(d._1) -> List[Constraint]())
    }
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

object CoherenceModel {
  def apply(block: => Unit): CoherenceModel = {
    new CoherenceModel {
      block
    }
  }
}


class CoherenceModel { self =>
  var P : scala.collection.mutable.Map[String,Proposition] = scala.collection.mutable.Map()
  var C : List[Constraint] = List()
  var CG : scala.collection.mutable.Map[Proposition,List[Constraint]] = scala.collection.mutable.Map()
  CoMod.coModel = this

  def subjectTo(c: List[Constraint]*) : CoherenceModel = {
    new CoherenceModel {
      P = self.P
      C = self.C ++ c.flatten
      CG = self.CG
    }
  }

  def evaluate(): Unit = {
    var oldActivations = scala.collection.mutable.Map[Proposition,Double]()

  }

  override def toString: String = {
    var str = s""
    for (p <- this.P) {
      str =str + s"Proposition: ${p._1}\nAdjacency list: \n"

      for (x <- this.CG(p._2)) {
        x match {
          case e : Explanation => str = str + s"${e.explanandum.id} --- ${e.explanan.id}  weight: ${e.weight} \n"
        }
      }
    }
    str
  }

}


