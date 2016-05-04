/**
  * Created by Romain Reuillon on 04/05/16.
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

import data._
import rule._
import world._
import cell._

import scala.util.Random

object Calibrate extends App {
  println(calibration.calibrate(0.5, 0.1, 0.7)(new Random(42)))
}


object calibration {

  case class Objectives(
    moran: Double,
    clusters: Double,
    activityInClusterRatio: Double)

  def evolutionRule(
    wayAttractivity: Double,
    intraMoveWeight: Double) = {
    //val initialIndustry = 0.5
    //val wayAttractivity = 1.1
    val peripheralNeigborhoudSize = 2


    /* Fonction de calcul de la valeur de centralité à partir de la fonction ci-dessus et de deux paramètes x,y*/
    def centrality = (grid: Grid) => concentricCentrality(grid)

    /* Transitions rules */
    val intraIndustry =
      urbanToUrbanRandomMove(Industry, centrality) -> intraMoveWeight

    val extraIndustry =
      urbanToNotUrbanRandomMove(
        Industry,
        wayAttractivity,
        peripheralNeigborhoudSize,
        centrality,
        (location, activity) => Urban(location, Vector(activity), Poor)
      ) -> (1 - intraMoveWeight)

    val industrialRevolution = multinomialChoice(intraIndustry, extraIndustry)

    val downgrade = downgradeNearIndustryHabitations(0.05)
    val upgrade = upgradeHabitations(0.01)

    industrialRevolution andThen downgrade andThen upgrade
  }


  def initialGrid(
  initialIndustry: Double)(rng: Random) =
    world.industrialWorld1(initialIndustry)(rng)


  /*
  * wayAttractivity: 1.0 -> 2.0
  * intraMoveWeight: 0.0 -> 1.0
  * initialIndustry: 0.1 -> 1.0
  */
  def calibrate(
     wayAttractivity: Double,
     intraMoveWeight: Double,
     initialIndustry: Double)(rng: Random) = {
    val model = evolutionRule(wayAttractivity, intraMoveWeight)
    val grid = initialGrid(initialIndustry)(rng)

    val numberOfIndustry =
      grid.cells.flatten.flatMap(urbanPrism.getOption).flatMap(_.activities.filter(_ == Industry)).size


    /* Simulate the dynamic */
    val finalGrid =
      dynamic.simulate(
        State(grid, rng),
        model,
        1000)

    val clusters = analyse.dbscan(finalGrid, Industry)

    Objectives(
      moran = analyse.moran(finalGrid, Industry),
      clusters = clusters.size,
      activityInClusterRatio = clusters.sum.toDouble / numberOfIndustry
    )
  }



}
