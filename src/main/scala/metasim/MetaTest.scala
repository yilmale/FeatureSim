package metasim

import scala.meta._

object MetaTest {
  def apply(): Unit = {

    val f =  source"""object featurea {

  abstract class Graph {
    def print(): Unit = {
      var x = 5
    }
  }


  abstract class Node {

  }


  abstract class Edge {

  }

}

object featureb {

  import Collaboration._


    refines {
      class Graph {

      }
    }

    refines {
      class Edge(n: featurea.Node) {

      }
    }

    refines {
      class Node
    }

    trait Weight {

    }

}""".collect { case cls: Defn.Object  => cls }


    println(f(1).children(1).children(2).children)
    println("-------------------")



    val y = "val x = 2".tokenize.get
    println(y)


    val myf = q"def myF() : Unit = {var lv : Int = 0}"


    myf match {
      case q"def $name(...$paramss): $result = { ..$body }" =>
        println(q"def $name: Unit = { ..$body }")
    }

    val m = q"case class User(name: String, age: Int)"
    println(m.name)

    println("object Main extends App { println(1) }".parse[Source].get)

    val c = source"""sealed trait Op[A]
    object Op extends B {
      case class Foo(i: Int) extends Op[Int] {var x : Int = 0}
      case class Bar(s: String) extends Op[String]
    }""".collect { case cls: Defn.Class => cls }

    println(c)

    println(c.head)

    println(c.head.tparams)

    val x1 = q"myList.filter(_ > 3 + a).headOption; myList1.filter(_ > 3 + a).headOption"

    println(x1)


    val x2 = x1.transform {
      case q"$lst.filter($cond).headOption" => q"$lst.find($cond)"
    }

    println(x2)

    val x3 = q"val x = 2;  class Z { def m1 : Unit = {}}"
    println(x3)


    val x4 = x3.transform {
      case q"class $lst {$body}" =>
        q"class $lst {$body; def m3 : Unit = {}} "
    }

    println(x4)

    var classes = source"""
      /** This is a docstring */
      class L {
         val v1 : Integer = 10
      }
      trait MyTrait // leading comment
      """.collect { case cls: Defn.Class => cls }

    println(classes.head)

  }
}
