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

object Model extends App {

  val initialIndustry = 0.5

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

  val rng = new Random(42)

  val wayAttractivity = 1.1
  val peripheralNeigborhoudSize = 2
  val grid = world.industrialWorld1(initialIndustry)(rng)


  /* Fonction de calcul de la valeur de centralité à partir de la fonction ci-dessus et de deux paramètes x,y*/
  def centrality: Centrality = (grid: Grid) => concentricCentrality(grid)

  /* Transitions rules */
  val intraIndustry = urbanToUrbanRandomMove(Industry, centrality) -> 0.1

  val extraIndustry =
    urbanToNotUrbanRandomMove(
      Industry,
      wayAttractivity,
      peripheralNeigborhoudSize,
      centrality,
      (location, activity) => Urban(location, Vector(activity), Poor)
    ) -> 0.9

  val industrialRevolution = multinomialChoice(intraIndustry, extraIndustry)

  val downgrade = downgradeNearIndustryHabitations(0.05)
  val upgrade = upgradeHabitations(0.01)

  val evolutionRule: Rule =
    industrialRevolution andThen downgrade andThen upgrade

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
          println(s"${analyse.averageDistance(s.grid, Industry)}, ${analyse.standardDeviation(s.grid, Industry)}, ${analyse.moran(s.grid, Industry)}, ${analyse.dbscan(s.grid, Industry).size}")
        case _ =>
     }

  def fileLogger(event: export.Logger.Event): Unit =
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
      State(grid, rng),
      evolutionRule,
      1000,
      distanceLogger)

//  println("-- Final --")
//  println(export.toCSV(centrality(finalGrid), finalGrid))

}


object Dynamic {

  def simulate(state: State, rule: Rule, steps: Int, logger: export.Logger.Logger) = {
    def simulate0(currentStep: Int, state: State): Grid = {
      logger(export.Logger.Step(currentStep, state.grid))
      if(currentStep >= steps) state.grid
      else {
        val newGrid = rule(state)
        simulate0(currentStep + 1, newGrid)
      }
    }

    simulate0(0, state)
  }

}








