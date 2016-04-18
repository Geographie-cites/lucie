/**
  * Created by Romain Reuillon on 22/03/16.
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Affero General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
package fr.geocites.lucie

import grid._
import fr.geocites.lucie.data._

object export {
  /* export en CSV*/
  def toCSV(centrality: PartialFunction[Location, Double], grid: Grid) = {
    val cellViews = List(cell.toCentralityCSV(centrality)(_), cell.toActivityCSV(_))
    val edges = grid.ways.map(Edge.toCSV).mkString(",")

    s"""${cellViews.map{ v => view(grid, v)}.mkString("\n\n")}
       |
      |$edges""".stripMargin
  }

  def view(grid: Grid, view: Cell => String) = {
    val csvGrid = grid.cells.map(_.map(view).mkString(",")).mkString("\n")

    s"""$csvGrid""".stripMargin
  }

  object Logger {
    sealed trait Event
    case class Step(step: Int, grid: Grid) extends Event

    type Logger = Event => Unit
  }
}
