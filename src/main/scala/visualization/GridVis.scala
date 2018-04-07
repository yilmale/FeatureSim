package visualization

import java.awt.{Color, Graphics, Graphics2D}


import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D

import javax.swing.JFrame
import javax.swing.JPanel

/**
  * Created by yilmaz on 12/6/16.
  */

trait Element {
  var eName : String
  val fName : String = "FTest"
  var eId : Int
}


class Element1 {
  var eName1 : String = ""
  var eId1 : Int = 5
}

class Element2 extends Element1 {
  eName1 = "test2"
}

class ELementTest {
  def elem() : Element = {
    new Element {
      var eName = "test"
      override val fName = "test1"
      var eId = 5
    }
  }

  def elem1() : Element1 = {
    new Element1 {
      eName1 = "test"
      eId1 = 5
    }
  }

  def elem2() : Element2 = {
    new Element2 {
      eName1 = "test"
      eId1 = 5
    }
  }
}




class GridVis extends JPanel {

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    var g2d = g.asInstanceOf[Graphics2D]

    g2d.setColor(new Color(212, 212, 212))
    for (y <- 0 to 25)
      for (x <- 0 to 25)
        g2d.drawRect(10+20*x, 15+20*y, 20, 20)


    g2d.setColor(new Color(31, 21, 1))

    val x = 5
    val y = 5
    g2d.fillRect(10+20*x, 15+20*y, 20, 20)

  }
}

object GridVis {
  def createGridVis(x : Int, y : Int) :GridVis = {
    val rects = new GridVis()
    val frame = new JFrame("Grid")
    //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.add(rects)
    frame.setSize(360, 300)
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
    rects
  }
}