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

  /**
    *  Fonction de calcul d'une valeur de centralité :
    *  distance au centre (centerX,centerY)
    *  normalisée entre 0 et 1 par rapport à la distance max au centre (citySide>Center)
    *
    * @param grid
    * @param centerX
    * @param centerY
    * @return
    */
  def concentricCentrality(grid: Grid, centerX: Int, centerY: Int): PartialFunction[Cell.Location, Double] = {
    def value(x: Int, y: Int): Double = {
      val max = (grid.side * math.sqrt(2)) / 2
      val relativeX = x - centerX
      val relativeY = y - centerY
      val distance = math.sqrt(relativeX * relativeX + relativeY * relativeY)
      (max - distance) / max
    }

    val centralities =
      for {
        c <- Grid.cells(grid)
      } yield
        c.get(grid) match {
          case u: Urban =>
            val (x, y) = u.location
            u.location -> value(x, y)
          case c => c.location -> 0.0
        }

    centralities.toMap
  }

  /* Fonction définition random d'un vecteur activité de type Industry ou vide */
  def activities(random: Random) =
    if(random.nextDouble() < 1.0) Vector(Industry) else Vector()

  /**
    * Fonction de génération de la grille de départ
    *
    * placement détermine de WATER
    * placement déterminé de URBAN > attribut activité généré depuis fonction qui génére les activités
    * cellules non URBAN = NONURBAN
    *
    * @param side side of the world
    * @return the world for stage1
    */
  def stage1(side: Int)(x: Int, y: Int)(implicit random: Random) =
    if(x == 0) Water(x -> y)
    else {
      if (x >= 3 && x <= 5 && y >= 7 && y <= 9) {
        val emptyCell = Urban(x -> y, activities = Vector())
        emptyCell.copy(activities = activities(random))
      } else NotUrban(x -> y)
    }

  implicit val rng = new Random(42)

  /* génération d'une grille de 11*11 cells*/
  val side = 11
  val grid = Grid.generate(side, stage1(side))

  /* Fonction de calcul de la valeur de centralité à partir de la fonction ci-dessus et de deux paramètes x,y*/
  def centrality(grid: Grid) = concentricCentrality(grid, centerX = 4, centerY = 8)

  /* Transitions rules */
  val intraIndustry = RuleBase(Dynamic.UrbanToUrbanRandomMove(Industry, centrality(grid)), 1.0)
  val extraIndustry = RuleBase(Dynamic.UrbanToNotUrbanRandomMove(Industry), 0.1)

  println(Grid.toCSV(centrality(grid), grid))

  /* Simulate the dynamic */
  val finalGrid = Dynamic.simulate(grid, Vector(intraIndustry, extraIndustry), 100)

  println("-- Final --")
  println(Grid.toCSV(centrality(finalGrid), finalGrid))

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

  /**
    * Randomly select origin of a move among cells containing a given activity
    *
    */
  def selectOriginWithActivity(grid: Grid, activity: Activity, random: Random) = {
    val origins =
      Grid.cells(grid).flatMap { cell =>
        cell.get(grid) match {
          case urban: Urban =>
            if(urban.activities.contains(activity)) Some(cell -> urban)
            else None
          case _ => None
        }
      }

    origins(random.nextInt(origins.size))
  }


  /**
    *  Move activity from an urban cell to another
    */
  case class UrbanToUrbanRandomMove(activity: Activity, centrality: PartialFunction[Cell.Location, Double]) extends Rule {
    def apply(grid: Grid, random: Random): Grid = {
      val (origin, urbanOrigin) = selectOriginWithActivity(grid, activity, random)

      /* Création d'une liste de cells URBAN de destination possibles */
      val destinations =
        Grid.cells(grid).filter(c => !Cell.hasSameLocation(c.get(grid), origin.get(grid))).flatMap {
          cell =>
            cell.get(grid) match {
              case urb: Urban =>
                val weight =  1 - centrality(urb.location)
                val element = cell -> urb
                Some((element, weight))
              case _ => None
            }
        }.toList

      /* Choose the destination at random given the weights */
      val (destination, urbanDestination) = multinomial(destinations.toList)(random)

      /* Update the grid by setting origin and destination with updated cells */
      (origin.set(Urban.removeActivity(urbanOrigin, activity)) andThen
        destination.set(Urban.addActivity(urbanDestination, activity))) (grid)
    }
  }

  /**
    * Move activity from a urban cell to a peripheral non-urban cell
    * It transform the destination cell into a urban cell
    */
  case class UrbanToNotUrbanRandomMove(activity: Activity) extends Rule {
    def apply(grid: Grid, random: Random): Grid = {

      val (origin, urbanOrigin) = selectOriginWithActivity(grid, activity, random)

      /* Select any non-urban cell on peripheral */
      // TODO add effect of transport network
      val peripheral =
        for {
          (cell, neighbours) <- Grid.neighboursCells(grid)
          if (cell.get(grid) match {
            case _: NotUrban => true
            case _ => false
          })
          if neighbours.exists {
            _.get(grid) match {
              case _: Urban => true
              case _ => false
            }
          }
        } yield cell

      /* Select the destination uniformly at random */
      val destination = peripheral(random.nextInt(peripheral.size))
      val destinationCell = destination.get(grid)

      (origin.set(Urban.removeActivity(urbanOrigin, activity)) andThen
        (destination.set(Urban(destinationCell.location, Vector(activity))))) (grid)
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
  def location: Cell.Location
}

// TODO add flow direction
case class Water(location: Cell.Location) extends Cell
/**
  * Urban cell
  *
  * @param activities list of activities of the urban cell
  */
case class Urban(location: Cell.Location, activities: Vector[Activity]) extends Cell
case class NotUrban(location: Cell.Location) extends Cell

object Urban {

  /**
    * Remove an activity from a urban cell
    * Identity if the activity has not been found in the cell
    */
  def removeActivity(urban: Urban, activity: Activity): Urban = {
    urban.activities.indexOf(activity) match {
      case -1 => urban
      case i =>
        val newActivities = urban.activities patch (from = i, patch = Nil, replaced = 1)
        urban.copy(activities = newActivities)
    }
  }

  /**
    * Add an activity in a urban cell
    */
  def addActivity(urban: Urban, activity: Activity) =
    urban.copy(activities = urban.activities ++ Seq(activity))

}



sealed trait Activity
case object Industry extends Activity

object Grid {

  def neighboursCells(grid: Grid) =
    for {
      (i, j) <- coordinates(grid.side)
    } yield cell(i, j) -> neighbours(grid.side, i, j).map { case(ni, nj) => Grid.cell(ni, nj) }

  /* définition d'un voisinage*/
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

  /* Renvoie un couple set / get qui remplace / renvoie un cell particuliére dans une grille */
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

  /* export en CSV*/
  def toCSV(centrality: PartialFunction[Cell.Location, Double], grid: Grid) = {
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

/* Définition d'une classe Grid, composé de vecteurs, de edges et de side*/
case class Grid(cells: Vector[Vector[Cell]], edges: Vector[Edge], side: Int)

object Edge {
  def toCSV(edge: Edge) =
    s"${edge.orientation},${edge.coordinate}"
}

object Cell {

  type Location = (Int, Int)

  def hasSameLocation(c1: Cell, c2: Cell) = c1.location == c2.location

  def toCentralityCSV(centrality: PartialFunction[Cell.Location, Double])(cell: Cell) =
    cell match {
      case _: Water => s"Water(${centrality(cell.location)})"
      case _: Urban => s"Urban(${centrality(cell.location)})"
      case _: NotUrban => s"NotUrban(${centrality(cell.location)})"
    }

  def toActivityCSV(cell: Cell) =
    cell match {
      case c: Urban => s"Activities(${c.activities.mkString(" & ")})"
      case _ => ""
    }
}


