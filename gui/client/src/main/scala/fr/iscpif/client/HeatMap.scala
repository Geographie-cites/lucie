package fr.iscpif.client

import fr.iscpif.scaladget.api.{BootstrapTags ⇒ bs}
import fr.iscpif.scaladget.api.svg.path._
import org.scalajs.dom.raw.MouseEvent
import rx.core._
import scalatags.JsDom
import scalatags.JsDom._
import scalatags.JsDom.all._
import bs._
import client.JsRxTags._
import org.scalajs.dom


/*
 * Copyright (C) 22/03/16 // mathieu.leclaire@openmole.org
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

object HeatMap {

  case class Margins(top: Int, right: Int, bottom: Int, left: Int)

  trait CellEdge

  object Top extends CellEdge

  object Bottom extends CellEdge

  object Right extends CellEdge

  object Left extends CellEdge

  trait EdgeStroke {
    def transform(p: JsDom.TypedTag[dom.svg.Path]): JsDom.TypedTag[dom.svg.Path]
  }

  object RedBold extends EdgeStroke {
    def transform(p: JsDom.TypedTag[dom.svg.Path]): JsDom.TypedTag[dom.svg.Path] = p(svgAttrs.stroke := "red", svgAttrs.strokeWidth := "3")
  }

  object Black extends EdgeStroke {
    def transform(p: JsDom.TypedTag[dom.svg.Path]): JsDom.TypedTag[dom.svg.Path] = p(svgAttrs.stroke := "black")
  }

  case class HeatMapCell(x: Int, y: Int, intensity: Double, text: String)

  case class HeatMapEdge(cell: HeatMapCell, cellEdge: CellEdge, edgeStroke: EdgeStroke)

  case class Point(x: Double, y: Double)

  def apply(nbLine: Int, nbCol: Int, cells: Seq[HeatMapCell], edges: Seq[HeatMapEdge]) = {
    val hm = new HeatMap(nbLine, nbCol)
    hm.cells() = cells
    hm.edges() = edges
    hm
  }
}


import HeatMap._

class HeatMap(nbLine: Int, nbCol: Int) {

  val cells = rx.Var(Seq[HeatMapCell]())
  val edges = rx.Var(Seq[HeatMapEdge]())

  val sceneWidth = 1200
  val sceneHeight = 800
  val margins = Margins(100, 100, 100, 100)
  val cellSize = Math.floor(sceneWidth / 24).toInt
  val tooltipOffsetLeft = margins.left - cellSize / 2
  val tooltipOffsetTop = margins.top - cellSize / 2
  val tooltipVisible: Var[Option[HeatMapCell]] = Var(None)

  lazy val svgNode =
    svgTags.svg(
      svgAttrs.width := sceneWidth + margins.left + margins.right,
      svgAttrs.height := sceneHeight + margins.top + margins.bottom,
      Rx {
        svgTags.g(
          svgAttrs.transform := s"translate( ${margins.left}, ${margins.top} )")(
          cells().map(cell): _*)(edges().map(edge): _*
        )
      }
    )


  def cell(hmc: HeatMapCell) =
    svgTags.rect(
      svgAttrs.x := hmc.x * cellSize,
      svgAttrs.y := hmc.y * cellSize,
      svgAttrs.rx := 1,
      svgAttrs.ry := 1,
      svgAttrs.width := cellSize,
      svgAttrs.height := cellSize,
      scalatags.JsDom.all.`class` := "heatMapRect",
      onmouseover := {
        (e: MouseEvent) =>
          tooltipVisible() = Some(hmc)
      }
    )


  def edge(heatMapEdge: HeatMapEdge) = {
    val x = heatMapEdge.cell.x * cellSize
    val y = heatMapEdge.cell.y * cellSize
    val right = x + cellSize
    val bottom = y + cellSize

    heatMapEdge.edgeStroke.transform(
      heatMapEdge.cellEdge match {
        case Top => start(x, y) h(right)
        case Bottom => start(x, bottom) h(right)
        case Right => start(right, y) v(right)
        case _ => start(x, y) v(bottom)
      }
    )
  }

  val tooltip = Rx {
    val (visible, top, left, text) = tooltipVisible() match {
      case Some(hmc) => ("", hmc.y * cellSize + tooltipOffsetTop, hmc.x * cellSize + tooltipOffsetLeft, hmc.text)
      case _ => ("hidden", -100, -100, "")
    }

    div(visible)(
      attrs.id := "tooltip",
      attrs.style := s"top: ${
        top
      }px; left: ${
        left
      }px;")(
      tags.p(text)
    )
  }


  org.scalajs.dom.document.body.appendChild(tooltip).render

}
