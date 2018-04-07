package communication

import agent.Message
import scala.xml._

/**
  * Created by yilmaz on 2/22/17.
  */


object Communication {

  def testComm(): scala.xml.Node = {
    val myM = new Message {
      var description = "Test"
      var sender = 4
    }

    val msgXML = myM.toXML

    println(msgXML)
    msgXML
  }

  def xmltoStr(m : scala.xml.Node): Unit = {

  }



}