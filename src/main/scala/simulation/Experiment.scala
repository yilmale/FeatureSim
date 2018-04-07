package simulation

/**
  * Created by Levent Yilmaz on 2/4/2017.
  */


trait Experiment  {
  var expType : String = _
  var params : Map[String,Any] = _
  var ctx : Context = _
  var observer : Observer = _
  ModelUtility.experiment = this

  def simulate(): Unit = {
    ctx.simulate
  }

}

trait AnalysisModel {
  def analyze() : Unit
}

trait Observer {
  var monitors : Map[String,Any]
  def monitor : Unit
}