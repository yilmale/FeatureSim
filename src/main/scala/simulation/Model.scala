package simulation

import agent.Agent
import visualization.{Space2D, SpaceVis}


/**
  * Created by Levent Yilmaz on 2/4/2017.
  */

class Model {

}

object ModelUtility {
  var experiment : Experiment = _
  var ctx : Context = _
  var modelName : String = _
  var gridView : Grid = _
  var spaceView : SpaceVis = _
  def setModelName(n : String): Unit = {
    modelName = n
  }
  def getModelName : String = {
    modelName
  }
  def setProjection(g:Grid): Unit = {
    gridView = g
  }

  def setSpace(s: SpaceVis): Unit = {
    spaceView = s
  }

  def register(m : Context): Unit = {
    ctx = m
  }

  def remove(a : Agent): Unit = {
    ctx remove a
    if (ctx.projectionDefined)
      gridView remove a
    if (ctx.spaceDefined)
      spaceView remove a

  }

}