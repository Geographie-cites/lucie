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
import better.files._
import monocle.macros._

object Model extends App {

  val initialIndustry = 0.5

  def concentricCentrality(grid: Grid): PartialFunction[Cell.Location, Double] = {
    def potentialMatrix(center: Cell) =
      Vector.tabulate(grid.side, grid.side) {
        (x, y) =>
          val d = Cell.distance(center.location, (x, y))
          1.0 / (1.0 + math.pow(d, 2.0))
      }

    def centers =
     Grid.cells(grid).filter {
       case u: Urban => u.activities.exists(_ == Center)
       case _ => false
     }

    def aggregatedMatrix = {
      val matrices = centers.map(potentialMatrix)

      Vector.tabulate(grid.side, grid.side) {
        (x, y) => (x, y) -> matrices.map(_(x)(y)).max
      }
    }

    aggregatedMatrix.flatten.toMap
  }

  /* Fonction définition random d'un vecteur activité de type Industry ou vide */
  def activities(random: Random) =
    if(random.nextDouble() < initialIndustry) Vector(Industry) else Vector()


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
      if (x >= 3 && x <= 9 && y >= 7 && y <= 13) {
        val acts =
          if(x == 6 && y == 10) activities(random) ++ Seq(Center)
          else activities(random)

        val level =
          if(acts.contains(Center)) Elite
          else if(acts.contains(Industry)) Poor
          else Middle

        Urban(x -> y, activities = acts, habitatLevel = level)
      } else NotUrban(x -> y)
    }

  implicit val rng = new Random(42)

  val wayAttractivity = 1.1
  val peripheralNeigborhoudSize = 2

  val side = 21
  val matrix =
    Vector.tabulate(side, side) {
      (i, j) => stage1(side)(i, j)
    }

  val edges = Vector(GenericWay(Vertical, 5), GenericWay(Horizontal, 4))

  val grid = Grid(matrix, edges, side)

  /* Fonction de calcul de la valeur de centralité à partir de la fonction ci-dessus et de deux paramètes x,y*/
  def centrality: Grid.Centrality = (grid: Grid) => concentricCentrality(grid)

  /* Transitions rules */
  val intraIndustry = Dynamic.urbanToUrbanRandomMove(Industry, centrality) -> 0.9

  val extraIndustry =
    Dynamic.urbanToNotUrbanRandomMove(
      Industry,
      wayAttractivity,
      peripheralNeigborhoudSize,
      centrality,
      (location, activity) => Urban(location, Vector(activity), Poor)
    ) -> 0.1


  val downgrade = Dynamic.downgradeNearIndustryHabitations(0.05)
  val upgrade = Dynamic.upgradeHabitations(0.01)

  val evolutionRule = new Rule {
    override def apply(grid: Grid, random: Random): Grid = {
      val composideRule =
        (Dynamic.multinomialChoice(intraIndustry, extraIndustry)(_: Grid, random)) andThen
          (downgrade(_, random)) andThen
          (upgrade(_, random))
      composideRule(grid)
    }
  }

  println(Grid.toCSV(centrality(grid), grid))

  val baseDir = File("/tmp/lucie/")

  type ColumnValue = (Int, Int) => String

   def toCSV(x: Int, y: Int)(values: ColumnValue*) = {
     def lines =
       for {
         i <- 0 until x
         j <- 0 until y
         vs = values.map(_(i, j))
       } yield (Seq(x - i - 1, j) ++ vs).mkString(",")
     lines.mkString("\n")
   }

  def logger(event: Logger.Event): Unit =
    event match {
      case s: Logger.Step =>

        val stepDir = baseDir / s.step.toString
        stepDir.createDirectories()

        def cellType(i: Int, j: Int) =
          s.grid.cells(i)(j) match {
            case _: Urban => "u"
            case _: NotUrban => "n"
            case _: Water => "w"
          }

        def industry(i: Int, j: Int) =
          s.grid.cells(i)(j) match {
            case u: Urban => u.activities.count(_ == Industry).toString
            case _ => "0"
          }

        def attractivity(i: Int, j: Int) =
          Dynamic.aggregatedAttractivity(s.grid, peripheralNeigborhoudSize, wayAttractivity)(i, j).toString

        val currentCentrality = centrality(s.grid)
        def gridCentrality(i: Int, j: Int) = currentCentrality(i, j).toString

        def level(i: Int, j: Int) = grid.cells(i)(j) match {
          case u: Urban => u.habitatLevel.toString
          case _ => ""
        }


        stepDir / "cells.csv" < Seq("x", "y", "type", "industry", "attractivity", "centrality", "level").mkString(",") + "\n"
        stepDir / "cells.csv" << toCSV(s.grid.side, s.grid.side)(cellType, industry, attractivity, gridCentrality, level)
    }

  baseDir.createDirectories()
  baseDir / "ways.csv" < grid.ways.map(Edge.toCSV).mkString("\n")
  baseDir / "parameters.csv" <
    s"""wayAttractivity,${wayAttractivity}
       |peripheralNeigborhoudSize,${peripheralNeigborhoudSize}
     """.stripMargin


  /* Simulate the dynamic */
  val finalGrid =
    Dynamic.simulate(
      grid,
      evolutionRule,
      100,
      logger)

  println("-- Final --")
  println(Grid.toCSV(centrality(finalGrid), finalGrid))

}


trait Rule extends ((Grid, Random) => Grid)

object Dynamic {

  def multinomialChoice(rules: (Rule, Double)*) = new Rule {
    override def apply(v1: Grid, v2: Random): Grid =  {
      val rule = multinomial(rules.map{ case(r, w) => (r, w) }.toList)(v2)
      rule(v1, v2)
    }
  }

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


  def randomCellWithActivity(grid: Grid, activity: Activity, random: Random, centrality: PartialFunction[Cell.Location, Double]) = {
    val selectedCells =
      Grid.cells(grid).collect { case (x: Urban) => x }.flatMap { urban =>
        if(urban.activities.contains(activity)) Some(urban -> centrality(urban.location))
        else None
      }

    multinomial(selectedCells.toList)(random)
  }


  /**
    *  Move activity from an urban cell to another
    */
  def urbanToUrbanRandomMove(activity: Activity, centrality: Grid.Centrality) = new Rule {
    def apply(grid: Grid, random: Random): Grid = {
      val gridCentrality = centrality(grid)

      val urbanOrigin = randomCellWithActivity(grid, activity, random, gridCentrality)

      /* Création d'une liste de cells URBAN de destination possibles */
      val destinations =
        Grid.cells(grid).
          filter(c => !Cell.hasSameLocation(c, urbanOrigin)).
          collect { case (x: Urban) => x }.map { urb =>
          val weight =  1 - gridCentrality(urb.location)
          (urb, weight)
        }

      /* Choose the destination at random given the weights */
      val urbanDestination = multinomial(destinations.toList)(random)

      /* Update the grid by setting origin and destination with updated cells */
      (Grid.lens(urbanOrigin).set(Urban.removeActivity(urbanOrigin, activity)) andThen
        Grid.lens(urbanDestination).set(Urban.addActivity(urbanDestination, activity))) (grid)
    }
  }

  def peripheralAttractivity(grid: Grid, peripheralNeigborhoudSize: Int)(x: Int, y: Int): Double =
    grid.cells(x)(y) match {
      case _: NotUrban =>
        def peripheral =
          Grid.neighbourCells(grid, x -> y, peripheralNeigborhoudSize).exists {
            case _: Urban => true
            case _ => false
          }
        if(peripheral) 1.0 else 0.0
      case _ => 0.0
    }

  def transportAttractivity(grid: Grid, wayAttractivity: Double)(x: Int, y: Int): Double = {
    def nearWay = grid.ways.exists {
      case GenericWay(orientation, c) =>
        orientation match {
          case Horizontal => c - 1 <= y && c >= y
          case Vertical =>  c - 1 <= x && c >= x
        }
    }
    if(nearWay) wayAttractivity else 1.0
  }

  def aggregatedAttractivity(grid: Grid, peripheralNeigborhoudSize: Int, wayAttractivity: Double)(x: Int, y: Int) =
    peripheralAttractivity(grid, peripheralNeigborhoudSize)(x, y) *
      transportAttractivity(grid, wayAttractivity)(x, y)

  /**
    * Move activity from a urban cell to a peripheral non-urban cell
    * It transform the destination cell into a urban cell
    */
  def urbanToNotUrbanRandomMove(
    activity: Activity,
    wayAttractivity: Double,
    peripheralNeigborhoudSize: Int,
    centrality: Grid.Centrality,
    buildUrbanCell: (Cell.Location, Activity) => Cell) = new Rule {

    def apply(grid: Grid, random: Random): Grid = {
      val gridCentrality = centrality(grid)
      val urbanOrigin = randomCellWithActivity(grid, activity, random, gridCentrality)

      /* Select the destination uniformly at random */
      val attractivityMatrix =
        Grid.cells(grid).map { c =>
          val (x, y) = c.location
          c -> (aggregatedAttractivity(grid, peripheralNeigborhoudSize, wayAttractivity)(x, y))
        }.collect { case x@(_: NotUrban, _) => x }

      val destination = multinomial(attractivityMatrix.toList)(random)

      (Grid.lens(urbanOrigin).set(Urban.removeActivity(urbanOrigin, activity)) andThen
        (Grid.lens(destination).set(buildUrbanCell(destination.location, activity)))) (grid)
    }
  }

  def downgradeNearIndustryHabitations(p: Double) = new Rule {
    override def apply(grid: Grid, rng: Random): Grid = {
      Grid.cells(grid).collect { case x: Urban => x }.foldLeft(grid) { (g, u) =>
        if (u.activities.exists(_ == Industry) && rng.nextDouble() < p)
          Grid.lens(u).set(u.copy(habitatLevel = HabitatLevel.downgrade(u.habitatLevel)))(g)
        else g
      }
    }
  }

  def upgradeHabitations(p: Double) = new Rule {
    override def apply(grid: Grid, rng: Random): Grid = {
      Grid.cells(grid).collect { case x: Urban => x }.foldLeft(grid) { (g, u) =>
        if (u.activities.forall(_ != Industry) && rng.nextDouble() < p)
          Grid.lens(u).set(u.copy(habitatLevel = HabitatLevel.upgrade(u.habitatLevel)))(g)
        else g
      }
    }
  }

  def simulate(grid: Grid, rule: Rule, steps: Int, logger: Logger.Logger)(implicit random: Random) = {

    def simulate0(currentStep: Int, grid: Grid): Grid = {
      logger(Logger.Step(currentStep, grid))
      if(currentStep >= steps) grid
      else {
        val newGrid = rule(grid, random)
        simulate0(currentStep + 1, newGrid)
      }
    }
    simulate0(0, grid)
  }

}


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
@Lenses case class Urban(location: Cell.Location, activities: Vector[Activity], habitatLevel: HabitatLevel) extends Cell
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

object HabitatLevel {

  def upgrade(l: HabitatLevel) =
    l match {
      case Elite => Elite
      case Middle => Elite
      case Poor => Middle
    }

  def downgrade(l: HabitatLevel) =
    l match {
      case Elite => Middle
      case Middle => Poor
      case Poor => Poor
    }

}

sealed trait Activity
case object Industry extends Activity
case object Center extends Activity
sealed trait HabitatLevel
case object Elite extends HabitatLevel
case object Middle extends HabitatLevel
case object Poor extends HabitatLevel


object Grid {

  type Centrality = Grid => PartialFunction[Cell.Location, Double]

  def neighbourCells(grid: Grid, l: Cell.Location, size: Int) =
    neighbours(grid.side, l, size).map {
      case(i, j) => grid.cells(i)(j)
    }

  /* définition d'un voisinage*/
  def neighbours(side: Int, location: Cell.Location, size: Int) = {
    val (i, j) = location

    for {
      di <- (-size to size)
      dj <- (-size to size)
      if (di != 0 && dj != 0)
      ni = i + di
      nj = j + dj
      if (ni >= 0 && nj >= 0 && ni < side && nj < side)
    } yield (i + di, j + dj)
  }

  def coordinates(side: Int) =
    for {
      i <- 0 until side
      j <- 0 until side
    } yield (i, j)

  def cellLenses(g: Grid) = coordinates(g.side).map { case(i, j) => lens(i, j) }
  def cells(g: Grid) = g.cells.flatten


  def lens(cell: Cell): monocle.Lens[Grid, Cell] = lens(cell.location)

  /* Renvoie un couple set / get qui remplace / renvoie un cell particuliére dans une grille */
  def lens(location: Cell.Location): monocle.Lens[Grid, Cell] = {
    val (x, y) = location
    monocle.Lens { (g: Grid) => g.cells(x)(y) } {
      c => g =>
        val line = g.cells(x)
        val newCells = g.cells.updated(x, line.updated(y, c))
        g.copy(cells = newCells)
    }
  }

  /* export en CSV*/
  def toCSV(centrality: PartialFunction[Cell.Location, Double], grid: Grid) = {
    val cellViews = List(Cell.toCentralityCSV(centrality)(_), Cell.toActivityCSV(_))
    val edges = grid.ways.map(Edge.toCSV).mkString(",")

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
case class Grid(cells: Vector[Vector[Cell]], ways: Vector[GenericWay], side: Int)

object Edge {
  def toCSV(edge: GenericWay) =
    s"${edge.orientation},${edge.coordinate}"
}

sealed trait Orientation
case object Horizontal extends Orientation
case object Vertical extends Orientation


case class GenericWay(orientation: Orientation, coordinate: Int)

object Cell {

  type Location = (Int, Int)

  def distance(l1: Location, l2: Location) =
    math.sqrt(math.pow(l2._1 - l1._1, 2) + math.pow(l2._2 - l1._2, 2))

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

object Logger {
  sealed trait Event
  case class Step(step: Int, grid: Grid) extends Event

  type Logger = Event => Unit
}


object Analyse {

  import org.apache.commons.math3.util.MathArrays

  /**
    * Moran index using fast convolution. Matrix should be a square matrix.
    */
  def moran(matrix: Vector[Vector[Double]]): Double = {
    val n = matrix.length
    val flatConf = matrix.flatten
    val popMean = flatConf.sum / flatConf.length
    val centeredConf = matrix.map { r => r.map { d => d - popMean }.toArray }.toArray
    val variance = MathArrays.ebeMultiply(centeredConf.flatten, centeredConf.flatten).sum
    val weights = spatialWeights(2 * n - 1)
    val totWeight = Convolution.convolution2D(Array.fill(n, n) { 1.0 }, weights).flatten.sum
    flatConf.length / (totWeight * variance) * MathArrays.ebeMultiply(centeredConf.flatten, Convolution.convolution2D(centeredConf, weights).flatten).sum
  }

  def spatialWeights(n: Int): Array[Array[Double]] = {
    Array.tabulate(n, n) { (i, j) => if (i == n / 2 && j == n / 2) 0.0 else 1 / Math.sqrt((i - n / 2) * (i - n / 2) + (j - n / 2) * (j - n / 2)) }
  }

}


object Convolution {

  import org.apache.commons.math3.complex.Complex
  import org.apache.commons.math3.transform.{ TransformType, DftNormalization, FastFourierTransformer }
  import org.apache.commons.math3.util.MathArrays
  import math._

  /**
    * Generic convol for double Arrays (in O(nlog(n)), using FFT)
    *
    * @param x
    * @param k centered kernel
    * @return y = x*k with y_i = \sum_{j=1}{|K|}{x_{i-j-|K|/2}*k_j}
    */
  def convolution(x: Array[Double], k: Array[Double]): Array[Double] = {
    val xl = pow(2.0, ceil(log(x.length) / log(2.0)) + 1)
    val xp = x.padTo(x.length + (xl.toInt - x.length) / 2, 0.0).reverse.padTo(xl.toInt, 0.0).reverse
    val kp = k.padTo(k.length + (xl.toInt - k.length) / 2, 0.0).reverse.padTo(xl.toInt, 0.0).reverse
    val tr = new FastFourierTransformer(DftNormalization.STANDARD)
    val ftx = tr.transform(xp, TransformType.FORWARD)
    val ftk = tr.transform(kp, TransformType.FORWARD)
    val real = MathArrays.ebeSubtract(MathArrays.ebeMultiply(ftx.map { z => z.getReal }, ftk.map { z => z.getReal }), MathArrays.ebeMultiply(ftx.map { z => z.getImaginary }, ftk.map { z => z.getImaginary }))
    val im = MathArrays.ebeAdd(MathArrays.ebeMultiply(ftx.map { z => z.getReal }, ftk.map { z => z.getImaginary }), MathArrays.ebeMultiply(ftx.map { z => z.getImaginary }, ftk.map { z => z.getReal }))
    val trinv = tr.transform(Array.tabulate(real.length) { i => new Complex(real(i), im(i)) }, TransformType.INVERSE).map { z => z.getReal }
    trinv.splitAt(trinv.length - x.length / 2)._2 ++ trinv.splitAt(x.length - x.length / 2)._1
  }

  /**
    * Square convol (for tests)
    *
    * @param x
    * @param k
    * @return
    */
  def directConvol(x: Array[Double], k: Array[Double]): Array[Double] = {
    val kl = k.length
    val xpadded = x.padTo(x.length + kl, 0.0).reverse.padTo(x.length + 2 * kl, 0.0).reverse
    Array.tabulate(x.length + k.length) { i => MathArrays.ebeMultiply(k.reverse, xpadded.splitAt(i + 1)._2.splitAt(k.length)._1).sum }
  }

  /**
    *  2D convolution
    *  Using bijection [|1,N|]2 ~ [|1,N|] by flattening, after having good paddling
    *
    * @param x
    * @param k
    */
  def convolution2D(x: Array[Array[Double]], k: Array[Array[Double]]): Array[Array[Double]] = {
    val xpad = x.map { row => row.padTo(2 * (row.length / 2) + 1 + k(0).length, 0.0).reverse.padTo(2 * (row.length / 2) + 1 + 2 * k(0).length, 0.0).reverse }.padTo(2 * (x.length / 2) + 1 + k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).reverse.padTo(2 * (x.length / 2) + 1 + 2 * k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 })
    val xpos = Array.fill(x.length, x(0).length) { 1.0 }.map { row => row.padTo(2 * (row.length / 2) + 1 + k(0).length, 0.0).reverse.padTo(2 * (row.length / 2) + 1 + 2 * k(0).length, 0.0).reverse }.padTo(2 * (x.length / 2) + 1 + k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).reverse.padTo(2 * (x.length / 2) + 1 + 2 * k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).flatten
    val kpad = k.map { row => row.padTo(row.length + (xpad(0).length - row.length) / 2, 0.0).reverse.padTo(xpad(0).length, 0.0).reverse }.padTo(k.length + (xpad.length - k.length) / 2, Array.fill(xpad(0).length) { 0.0 }).reverse.padTo(xpad.length, Array.fill(xpad(0).length) { 0.0 })
    val flatconv = convolution(xpad.flatten, kpad.flatten)
    flatconv.zipWithIndex.filter { case (_, j) => xpos(j) > 0 }.map { case (d, _) => d }.sliding(x(0).length, x.length).toArray.reverse
  }

}

