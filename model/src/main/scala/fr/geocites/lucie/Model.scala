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
import rule._
import cell._
import fr.geocites.lucie.data._
import grid._


object Test {

  val initialIndustry = 0.5

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

  def grid(implicit random: Random) = {
    val side = 21
    val matrix =
      Vector.tabulate(side, side) {
        (i, j) => Test.stage1(side)(i, j)
      }

    val edges = Vector(GenericWay(Vertical, 5), GenericWay(Horizontal, 4))

    Grid(matrix, edges, side)
  }
}

object Model extends App {

  def concentricCentrality(grid: Grid): PartialFunction[Location, Double] = {
    def potentialMatrix(center: Cell) =
      Vector.tabulate(grid.side, grid.side) {
        (x, y) =>
          val d = distance(center.location, (x, y))
          1.0 / (1.0 + math.pow(d, 2.0))
      }

    def centers =
      cells(grid).filter {
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


  implicit val rng = new Random(42)

  val wayAttractivity = 1.1
  val peripheralNeigborhoudSize = 2

  val grid = Test.grid


  /* Fonction de calcul de la valeur de centralité à partir de la fonction ci-dessus et de deux paramètes x,y*/
  def centrality: Centrality = (grid: Grid) => concentricCentrality(grid)

  /* Transitions rules */
  val intraIndustry = urbanToUrbanRandomMove(Industry, centrality) -> 0.9

  val extraIndustry =
    urbanToNotUrbanRandomMove(
      Industry,
      wayAttractivity,
      peripheralNeigborhoudSize,
      centrality,
      (location, activity) => Urban(location, Vector(activity), Poor)
    ) -> 0.1


  val downgrade = downgradeNearIndustryHabitations(0.05)
  val upgrade = upgradeHabitations(0.01)

  val evolutionRule = new Rule {
    override def apply(grid: Grid, random: Random): Grid = {
      val composideRule =
        (multinomialChoice(intraIndustry, extraIndustry)(_: Grid, random)) andThen
          (downgrade(_, random)) andThen
          (upgrade(_, random))
      composideRule(grid)
    }
  }

  //println(export.toCSV(centrality(grid), grid))

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

  def distanceLogger(event: export.Logger.Event): Unit =
     event match {
        case s: export.Logger.Step =>
          println(s"${analyse.averageDistance(s.grid, Industry)}, ${analyse.standardDeviation(s.grid, Industry)}")
        case _ =>
     }

  def logger(event: export.Logger.Event): Unit =
    event match {
      case s: export.Logger.Step =>
        val stepDir = baseDir / s.step.formatted("%04d").toString
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
          aggregatedAttractivity(s.grid, peripheralNeigborhoudSize, wayAttractivity)(i, j).toString

        val currentCentrality = centrality(s.grid)
        def gridCentrality(i: Int, j: Int) = currentCentrality(i, j).toString

        def level(i: Int, j: Int) = s.grid.cells(i)(j) match {
          case u: Urban => u.habitatLevel.toString
          case _ => ""
        }

        stepDir / "cells.csv" < Seq("x", "y", "type", "industry", "attractivity", "centrality", "level").mkString(",") + "\n"
        stepDir / "cells.csv" << toCSV(s.grid.side, s.grid.side)(cellType, industry, attractivity, gridCentrality, level)
    }

//  baseDir.createDirectories()
//  baseDir / "ways.csv" < grid.ways.map(Edge.toCSV).mkString("\n")
//  baseDir / "parameters.csv" <
//    s"""wayAttractivity,${wayAttractivity}
//       |peripheralNeigborhoudSize,${peripheralNeigborhoudSize}
//     """.stripMargin


  /* Simulate the dynamic */
  val finalGrid =
    Dynamic.simulate(
      grid,
      evolutionRule,
      100,
      distanceLogger)

//  println("-- Final --")
//  println(export.toCSV(centrality(finalGrid), finalGrid))

}


object Dynamic {

  def simulate(grid: Grid, rule: Rule, steps: Int, logger: export.Logger.Logger)(implicit random: Random) = {

    def simulate0(currentStep: Int, grid: Grid): Grid = {
      logger(export.Logger.Step(currentStep, grid))
      if(currentStep >= steps) grid
      else {
        val newGrid = rule(grid, random)
        simulate0(currentStep + 1, newGrid)
      }
    }

    simulate0(0, grid)
  }

}








