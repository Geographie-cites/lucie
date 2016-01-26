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

  def concentricCentrality(citySide: Int, centerX: Int, centerY: Int)(x: Int, y: Int): Double = {
    val max = (citySide * math.sqrt(2)) / 2
    val relativeX = x - centerX
    val relativeY = y - centerY
    val distance = math.sqrt(relativeX * relativeX + relativeY * relativeY)
    (max - distance) / max
  }

  def centrality = concentricCentrality(citySide = 3, centerX = 4, centerY = 8)(_, _)

  def activities(random: Random) =
    if(random.nextDouble() < 0.5) Vector(Industry) else Vector()

  def stage1(side: Int)(x: Int, y: Int)(implicit random: Random) =
    if(x == 0) Water
    else {
      if(x >= 3 && x <= 5 && y >= 7 && y <= 9) {
        val emptyCell = Urban(centrality = centrality(x, y), activities = Vector())
        emptyCell.copy(activities = activities(random))
      } else NotUrban
    }

  implicit val rng = new Random(42)

  val side = 11
  val grid = Grid.generate(side, stage1(side))

  val intraIndustry = RuleBase(Dynamic.UrbanToUrbanRandomMove(Industry), 1.0)
  val extraIndustry = RuleBase(Dynamic.UrbanToNotUrbanRandomMove(Industry, centrality), 0.1)

  val finalGrid = Dynamic.simulate(grid, Vector(intraIndustry, extraIndustry), 100)

  println(Grid.toCSV(grid))

  println("-- Final --")

  println(Grid.toCSV(finalGrid))

}

case class RuleBase(rule: Rule, weight: Double)
trait Rule extends ((Grid, Random) => Grid)


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

  object RandomMove {
    def selectOrigin(grid: Grid, activity: Activity, random: Random) = {
      val origins =
        grid.coordinates.flatMap { case c@(i, j) =>
          grid(i, j) match {
            case cell@Urban(_, activities) =>
              if(activities.contains(activity)) Some((c, cell)) else None
            case _ => None
          }
        }

      origins(random.nextInt(origins.size))
    }
  }

  case class UrbanToUrbanRandomMove(activity: Activity) extends Rule {
    def apply(grid: Grid, random: Random): Grid = {
      val ((ox, oy), origin) = RandomMove.selectOrigin(grid, activity, random)

      val destinations: List[(((Int, Int), Urban), Double)] =
        grid.coordinates.filter(_ != (ox, oy)).flatMap { case c@(i, j) =>
          grid(i, j) match {
            case cell@Urban(centrality, _) => Some(((c, cell), 1 - centrality))
            case _ => None
          }
        }.toList


      val ((dx, dy), destination) = multinomial(destinations.toList)(random)

      val updatedOrigin = grid.update(ox, oy)(Urban.removeActivity(origin, activity))
      val updatedGrid = updatedOrigin.update(dx, dy)(Urban.addActivity(destination, activity))

      updatedGrid
    }
  }

  case class UrbanToNotUrbanRandomMove(activity: Activity, centrality: (Int, Int) => Double) extends Rule {
    def apply(grid: Grid, random: Random): Grid = {
      val ((ox, oy), origin) = RandomMove.selectOrigin(grid, activity, random)

      val peripheral = grid.coordinates.filter { case(i, j) =>
        grid.neighbours(i, j).exists { case(i, j) =>
          grid(i, j) match {
            case _: Urban => true
            case _ => false
          }
        }
      }


      val (dx, dy) = peripheral(random.nextInt(peripheral.size))

      val updatedOrigin = grid.update(ox, oy)(Urban.removeActivity(origin, activity))
      val updatedGrid = updatedOrigin.update(dx, dy)(Urban(centrality = centrality(dx, dy), activities = Vector(activity)))

      updatedGrid
    }
  }

  def simulate(grid: Grid, rules: Vector[RuleBase], steps: Int)(implicit random: Random) = {
    def selectRule = multinomial(rules.map{ case RuleBase(r, w) => (r, w)}.toList)

    def simulate0(currentStep: Int, grid: Grid): Grid =
      if(currentStep >= steps) grid
      else {
        val appliedRule = selectRule
        val newGrid = appliedRule(grid, random)
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

case class Urban(centrality: Double, activities: Vector[Activity]) extends Cell

sealed trait Activity
case object Industry extends Activity

case object NotUrban extends Cell {
  def centrality = 0
}

case class Grid(cells: Vector[Vector[Cell]], edges: Vector[Edge], side: Int) {

  def neighboursCells(i: Int, j: Int) =
    for {
      (ni, nj) <- neighbours(i, j)
    } yield apply(ni, nj)

  def neighbours(i: Int, j: Int) =
    for {
      di <- (-1 to 1)
      dj <- (-1 to 1)
      if(di != 0 && dj != 0)
      ni = i + di
      nj = j + dj
      if(ni >= 0 && nj >= 0 && ni < side && nj < side)
    } yield (i + di, j + dj)

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
