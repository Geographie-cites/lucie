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

object Model extends App {

  def concentricDensity(citySide: Int, centerX: Int, centerY: Int)(x: Int, y: Int) = {
    val max = (citySide * math.sqrt(2)) / 2
    val relativeX = x - centerX
    val relativeY = y - centerY

    val distance = math.sqrt(relativeX * relativeX + relativeY * relativeY)
    Urban(max - distance, List(Industry))
  }

  def stage1(side: Int)(x: Int, y: Int) =
    if(x == 0) Water
    else {
      if(x >= 3 && x <= 5 && y >= 7 && y <= 9)
        concentricDensity(citySide = 3, centerX = 4, centerY = 8)(x, y)
      else NotUrban
    }

  val side = 11
  val grid = Grid.generate(side, stage1(side))

  println(Grid.toCSV(grid))

}

sealed trait Orientation
case object Horizontal extends Orientation
case object Vertical extends Orientation

case class Edge(orientation: Orientation, coordinate: Int)



sealed trait Cell {
  def density: Double
}

case object Water extends Cell {
  def density = 0
}

case class Urban(density: Double, activities: List[Activity]) extends Cell

sealed trait Activity
case object Industry extends Activity



case object NotUrban extends Cell {
  def density = 0
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
  def toDensityCSV(cell: Cell) =
    cell match {
      case Water => "Water"
      case Urban(density, _) => s"Urban($density)"
      case NotUrban => "NotUrban"
    }

  def toActivityCSV(cell: Cell) =
    cell match {
      case Urban(_, activities) => s"Activities(${activities.mkString(",")})"
      case _ => ""
    }
}

object Grid {

  def generate(side: Int, density: (Int, Int) => Cell) = {
    val cells =
      Vector.tabulate(side, side) {
        (i, j) => density(i, j)
      }

    Grid(cells, Vector.empty, side)
  }

  def toCSV(grid: Grid) = {
    val cellViews = List(Cell.toDensityCSV(_), Cell.toActivityCSV(_))
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
