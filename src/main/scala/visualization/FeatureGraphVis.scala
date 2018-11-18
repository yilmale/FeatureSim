/*

package visualization

import java.util

import javax.swing.JFrame
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph
import com.mxgraph.layout.mxOrganicLayout
import com.mxgraph.layout.mxGraphLayout
import coherence._
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.model.mxCell

import scala.collection.mutable



class FeatureGraphVis(var cmod: CoherenceModel) extends
                    JFrame("Feature Graph")  {
  var graph = new mxGraph()
  var parentC = graph.getDefaultParent()

  graph.getModel().beginUpdate()

  try {

    for (p <- cmod.P values) {
      if (p.activation > 0)
        graph.insertVertex(parentC, p.id, p.id,
          20, 20, 100, 45,
          "shape=ellipse;fillColor=#00FFFF")
      else
        graph.insertVertex(parentC, p.id, p.id,
          20, 20, 100, 45,
          "shape=ellipse;fillColor=#FF0000")
    }
    var children = graph.getChildCells(parentC)
    for (e <- cmod.C) {
      e match {
        case e : Facilitation => {
          var src = e.action.id
          var tgt = e.goal.id
          var source : mxCell = null
          var target : mxCell = null
          for (x <- children) {
            var v1 = x.asInstanceOf[mxCell]
            var vname = v1.getId()
            if (vname == src) source = v1
            if (vname == tgt) target = v1
          }

          if ((source != null) && (target != null)) {
            if (e.weight > 0)
               graph.insertEdge(parentC,e.action.id+"-+-"+e.goal.id,"+",source,target)
            else
               graph.insertEdge(parentC,e.action.id+"---"+e.goal.id,"-",source,target)
          }
        }
      }

    }
  }
  finally {
      graph.getModel().endUpdate()
    }

   var children = graph.getChildCells(parentC)
   for (x <- children) println(x.asInstanceOf[mxCell].getId())

  var graphComponent = new mxGraphComponent(graph)
  getContentPane().add(graphComponent)
  layoutGraph()


  def layoutGraph(): Unit = {
    //var layout = new mxOrganicLayout(graph)
    var layout = new mxHierarchicalLayout(graph)
    var cell = graph.getDefaultParent()
    graph.getModel().beginUpdate()
    try {
      layout.execute(cell)
    }
    finally {
      graph.getModel().endUpdate()
    }
  }
}

*/