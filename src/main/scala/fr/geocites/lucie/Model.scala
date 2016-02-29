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

  def concentricCentrality(grid: Grid, centerX: Int, centerY: Int): PartialFunction[Cell, Double] = {
    def value(x: Int, y: Int): Double = {
      val max = (grid.side * math.sqrt(2)) / 2
      val relativeX = x - centerX
      val relativeY = y - centerY
      val distance = math.sqrt(relativeX * relativeX + relativeY * relativeY)
      (max - distance) / max
    }

    val centralities =
      for {
        (i, j) <- Grid.coordinates(grid.side)
      } yield {
        val c =  grid.cells(i)(j)
        c match {
          case _: Urban => c -> value(i, j)
          case _ => c -> 0.0
        }
      }

    centralities.toMap
  }

  def activities(random: Random) =
    if(random.nextDouble() < 0.5) Vector(Industry) else Vector()

  def stage1(side: Int)(x: Int, y: Int)(implicit random: Random) =
    if(x == 0) Water
    else {
      if(x >= 3 && x <= 5 && y >= 7 && y <= 9) {
        val emptyCell = Urban(activities = Vector())
        emptyCell.copy(activities = activities(random))
      } else NotUrban
    }

  implicit val rng = new Random(42)

  val side = 11
  val grid = Grid.generate(side, stage1(side))

  def centrality = concentricCentrality(grid, centerX = 4, centerY = 8)

  val intraIndustry = RuleBase(Dynamic.UrbanToUrbanRandomMove(Industry, centrality), 1.0)
  val extraIndustry = RuleBase(Dynamic.UrbanToNotUrbanRandomMove(Industry), 0.1)

  val finalGrid = Dynamic.simulate(grid, Vector(intraIndustry, extraIndustry), 100)

  println(Grid.toCSV(centrality, grid))

  println("-- Final --")

  println(Grid.toCSV(centrality, finalGrid))

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
        Grid.cells(grid).flatMap { cell =>
          cell.get(grid) match {
            case urban@Urban(activities) =>
              if(activities.contains(activity)) Some(cell -> urban)
              else None
            case _ => None
          }
        }

      origins(random.nextInt(origins.size))
    }
  }

  case class UrbanToUrbanRandomMove(activity: Activity, centrality: PartialFunction[Cell, Double]) extends Rule {
    def apply(grid: Grid, random: Random): Grid = {
      val (origin, urbanOrigin) = RandomMove.selectOrigin(grid, activity, random)

      val destinations =
        Grid.cells(grid).filter(_.get(grid) != origin.get(grid)).flatMap {
          cell =>
            cell.get(grid) match {
              case urb: Urban => Some((cell -> urb, 1 - centrality(urb)))
              case _ => None
            }
        }.toList

      val (destination, urbanDestination) = multinomial(destinations.toList)(random)

      (origin.set(Urban.removeActivity(urbanOrigin, activity)) andThen
        destination.set(Urban.addActivity(urbanDestination, activity))) (grid)
    }
  }

  case class UrbanToNotUrbanRandomMove(activity: Activity) extends Rule {
    def apply(grid: Grid, random: Random): Grid = {
      val (origin, urbanOrigin) = RandomMove.selectOrigin(grid, activity, random)

      val peripheral =
        for {
          (cell, neighbours) <- Grid.neighboursCells(grid)
          if neighbours.exists {
            _.get(grid) match {
              case _: Urban => true
              case _ => false
            }
          }
        } yield cell


      val destination = peripheral(random.nextInt(peripheral.size))

      (origin.set(Urban.removeActivity(urbanOrigin, activity)) andThen
        destination.set(Urban(Vector(activity)))) (grid)
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

sealed trait Cell

case object Water extends Cell


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

case class Urban(activities: Vector[Activity]) extends Cell

sealed trait Activity
case object Industry extends Activity
case object NotUrban extends Cell

object Grid {

  def neighboursCells(grid: Grid) =
    for {
      (i, j) <- coordinates(grid.side)
    } yield cell(i, j) -> neighbours(grid.side, i, j).map { case(ni, nj) => Grid.cell(ni, nj) }

  def neighbours(side: Int, i: Int, j: Int) =
    for {
      di <- (-1 to 1)
      dj <- (-1 to 1)
      if(di != 0 && dj != 0)
      ni = i + di
      nj = j + dj
      if(ni >= 0 && nj >= 0 && ni < side && nj < side)
    } yield (i + di, j + dj)

  def coordinates(side: Int) =
    for {
      i <- 0 until side
      j <- 0 until side
    } yield (i, j)

  def cells(g: Grid) = coordinates(g.side).map { case(i, j) => cell(i, j) }

  def cell(x: Int, y: Int): monocle.Lens[Grid, Cell] =
    monocle.Lens { (g: Grid) => g.cells(x)(y) } {
      c => g =>
        val line = g.cells(x)
        val newCells = g.cells.updated(x, line.updated(y, c))
        g.copy(cells = newCells)
    }

  def generate(side: Int, centrality: (Int, Int) => Cell) = {
    val cells =
      Vector.tabulate(side, side) {
        (i, j) => centrality(i, j)
      }

    Grid(cells, Vector.empty, side)
  }

  def toCSV(centrality: PartialFunction[Cell, Double], grid: Grid) = {
    val cellViews = List(Cell.toCentralityCSV(centrality)(_), Cell.toActivityCSV(_))
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

case class Grid(cells: Vector[Vector[Cell]], edges: Vector[Edge], side: Int)

object Edge {
  def toCSV(edge: Edge) =
    s"${edge.orientation},${edge.coordinate}"
}

object Cell {
  def toCentralityCSV(centrality: PartialFunction[Cell, Double])(cell: Cell) =
    cell match {
      case Water => s"Water(${centrality(cell)})"
      case Urban(_) => s"Urban(${centrality(cell)})"
      case NotUrban => s"NotUrban(${centrality(cell)})"
    }

  def toActivityCSV(cell: Cell) =
    cell match {
      case Urban(activities) => s"Activities(${activities.mkString(" & ")})"
      case _ => ""
    }
}


