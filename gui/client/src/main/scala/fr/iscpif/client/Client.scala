package client

import fr.iscpif.client.HeatMap
import fr.iscpif.client.HeatMap._
import fr.iscpif.scaladget.api.{BootstrapTags => bs}
import org.scalajs.dom
import bs._

import scala.concurrent.Future

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import shared._
import upickle._
import autowire._

import scalatags.JsDom.all._
import fr.geocites.lucie.data._

@JSExport("Client")
object Client {


  @JSExport
  def run() {



    val cells = Seq(
      HeatMapCell(1, 1, 23, "un"),
      HeatMapCell(1, 2, 23, "deux"),
      HeatMapCell(1, 3, 23, "trois"),
      HeatMapCell(2, 1, 23, "four"),
      HeatMapCell(2, 2, 23, "five"),
      HeatMapCell(2, 3, 23, "sechs")
    )

    val edges = cells.map{c=> HeatMapEdge(c, Bottom, RedBold)} ++ cells.map{c=> HeatMapEdge(c, Left, Black)}

    val heatMap = HeatMap(12, 20, cells, edges)
    dom.document.body.appendChild(heatMap.svgNode.render)
    
//    val cells2 = Seq(
//      HeatMapCell(1, 1, 23, "a text"),
//      HeatMapCell(1, 2, 23, "another text"))
//
//    heatMap.cells() = cells2

    Post[shared.Api].state.call().foreach{ g: Grid =>
      heatMap.cells() = for {
        c <- g.cells.flatten
        (x, y) = c.location
      } yield {
        val i =
          c match {
            case _: Urban => 1
            case _: Water => 50
            case _: NotUrban => 100
          }

        HeatMapCell(x, y, i, "")
      }
    }
  }

}

object Post extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {

  override def doCall(req: Request): Future[String] = {
    val url = req.path.mkString("/")
    dom.ext.Ajax.post(
      url = "http://localhost:8080/" + url,
      data = upickle.default.write(req.args)
    ).map {
      _.responseText
    }
  }

  def read[Result: upickle.default.Reader](p: String) = upickle.default.read[Result](p)

  def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
}
