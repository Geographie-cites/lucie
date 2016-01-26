/**
  * Created by Romain Reuillon on 26/01/16.
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

import scala.util.Random

object Model extends App {

  def concentricCentrality(citySide: Int, centerX: Int, centerY: Int)(x: Int, y: Int) = {
    val max = (citySide * math.sqrt(2)) / 2
    val relativeX = x - centerX
    val relativeY = y - centerY

    val distance = math.sqrt(relativeX * relativeX + relativeY * relativeY)
    Urban(max - distance, List.empty)
  }

  def activities(random: Random) =
    if(random.nextDouble() < 0.5) List(Industry) else List.empty

  def stage1(side: Int)(x: Int, y: Int)(implicit random: Random) =
    if(x == 0) Water
    else {
      if(x >= 3 && x <= 5 && y >= 7 && y <= 9) {
        val emptyCell = concentricCentrality(citySide = 3, centerX = 4, centerY = 8)(x, y)
        emptyCell.copy(activities = activities(random))
      } else NotUrban
    }

  implicit val rng = new Random(42)

  val side = 11
  val grid = Grid.generate(side, stage1(side))

  println(Grid.toCSV(grid))

}

sealed trait Orientation
case object Horizontal extends Orientation
case object Vertical extends Orientation

case class Edge(orientation: Orientation, coordinate: Int)

sealed trait Cell {
  def centrality: Double
}

case object Water extends Cell {
  def centrality = 0
}

case class Urban(centrality: Double, activities: List[Activity]) extends Cell

sealed trait Activity
case object Industry extends Activity

case object NotUrban extends Cell {
  def centrality = 0
}

case class Grid(cells: Vector[Vector[Cell]], edges: Vector[Edge], side: Int) {

  def update(x: Int, y: Int)(c: Cell) = {
    val line = cells(x)
    val newCells = cells.updated(x, line.updated(y, c))
    copy(cells = newCells)
  }

  def apply(x: Int, y: Int) = cells(x)(y)

}

object Edge {
  def toCSV(edge: Edge) =
    s"${edge.orientation},${edge.coordinate}"
}

object Cell {
  def toCentralityCSV(cell: Cell) =
    cell match {
      case Water => "Water"
      case Urban(centrality, _) => s"Urban($centrality)"
      case NotUrban => "NotUrban"
    }

  def toActivityCSV(cell: Cell) =
    cell match {
      case Urban(_, activities) => s"Activities(${activities.mkString(",")})"
      case _ => ""
    }
}

object Grid {

  def generate(side: Int, centrality: (Int, Int) => Cell) = {
    val cells =
      Vector.tabulate(side, side) {
        (i, j) => centrality(i, j)
      }

    Grid(cells, Vector.empty, side)
  }

  def toCSV(grid: Grid) = {
    val cellViews = List(Cell.toCentralityCSV(_), Cell.toActivityCSV(_))
    val edges = grid.edges.map(Edge.toCSV).mkString(",")

    s"""${cellViews.map{ v => Grid.view(grid, v)}.mkString("\n\n")}
      |
      |$edges""".stripMargin
  }

  def view(grid: Grid, view: Cell => String) = {
    val csvGrid = grid.cells.map(_.map(view).mkString(",")).mkString("\n")

    s"""$csvGrid""".stripMargin
  }

}
