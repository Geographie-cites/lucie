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

import scala.annotation.tailrec
import scala.util.Random

object Model extends App {

  def concentricCentrality(citySide: Int, centerX: Int, centerY: Int)(x: Int, y: Int) = {
    val max = (citySide * math.sqrt(2)) / 2
    val relativeX = x - centerX
    val relativeY = y - centerY
    val distance = math.sqrt(relativeX * relativeX + relativeY * relativeY)
    Urban((max - distance) / max, List.empty)
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

  val finalGrid = Dynamic.simulate(grid, Vector(Dynamic.randomMove(_, Industry)), 10)

  println(Grid.toCSV(grid))

  println("-- Final --")

  println(Grid.toCSV(finalGrid))

}






object Dynamic {

  def multinomial[T](values: List[(T, Double)])(implicit random: Random): T = {
    @tailrec def multinomial0[T](values: List[(T, Double)])(draw: Double): T = {
      values match {
        case Nil ⇒ throw new RuntimeException("List should never be empty.")
        case (bs, _) :: Nil ⇒ bs
        case (bs, weight) :: tail ⇒
          if (draw <= weight) bs
          else multinomial0(tail)(draw - weight)
      }
    }

    multinomial0(values)(random.nextDouble() * values.map(_._2).sum)
  }

  def randomMove(grid: Grid, activity: Activity)(implicit random: Random): Grid = {
    val origins =
      grid.coordinates.flatMap { case c@(i, j) =>
        grid(i, j) match {
          case cell@Urban(_, activities) =>
            if(activities.contains(activity)) Some((c, cell)) else None
          case _ => None
        }
      }

    val ((ox, oy), origin) = origins(random.nextInt(origins.size))

    val destinations: List[(((Int, Int), Urban), Double)] =
      grid.coordinates.flatMap { case c@(i, j) =>
        grid(i, j) match {
          case cell@Urban(centrality, _) => Some(((c, cell), 1 - centrality))
          case _ => None
        }
      }.toList


    val ((dx, dy), destination) = multinomial(destinations.toList)(random)

    val updatedOrigin = grid.update(ox, oy)(Urban.removeActivity(origin, activity))
    updatedOrigin.update(dx, dy)(Urban.addActivity(destination, activity))
  }


  def simulate(grid: Grid, rules: Vector[Grid => Grid], steps: Int)(implicit random: Random) = {
    def simulate0(currentStep: Int, grid: Grid): Grid =
      if(currentStep >= steps) grid
      else {
        val appliedRule = rules(random.nextInt(rules.size))
        val newGrid = appliedRule(grid)
        simulate0(currentStep +1, newGrid)
      }
    simulate0(0, grid)
  }

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


object Urban {

  def removeActivity(urban: Urban, activity: Activity): Urban = {
    urban.activities.indexOf(activity) match {
      case -1 => urban
      case i =>
        val newActivities = urban.activities patch (from = i, patch = Nil, replaced = 1)
        urban.copy(activities = newActivities)
    }
  }

  def addActivity(urban: Urban, activity: Activity) =
    urban.copy(activities = urban.activities ++ Seq(activity))

}

case class Urban(centrality: Double, activities: List[Activity]) extends Cell

sealed trait Activity
case object Industry extends Activity

case object NotUrban extends Cell {
  def centrality = 0
}

case class Grid(cells: Vector[Vector[Cell]], edges: Vector[Edge], side: Int) {

  def coordinates =
    for {
      i <- 0 until side
      j <- 0 until side
    } yield (i, j)

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
      case Urban(_, activities) => s"Activities(${activities.mkString(" & ")})"
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
