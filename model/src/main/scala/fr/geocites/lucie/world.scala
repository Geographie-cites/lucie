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
import grid._
import cell._
import scala.util.Random

object world {

  /**
    * Fonction de génération de la grille de départ
    *
    * placement détermine de WATER
    * placement déterminé de URBAN > attribut activité généré depuis fonction qui génére les activités
    * cellules non URBAN = NONURBAN
    *
    * @return the world for stage1
    */
  def industrialWorld1(initialIndustry: Double)(implicit random: Random) = {
    val side = 21

    /* Fonction définition random d'un vecteur activité de type Industry ou vide */
    def activities(random: Random) =
      if(random.nextDouble() < initialIndustry) Vector(Industry) else Vector()

    def cell(x: Int, y: Int)(implicit random: Random) =
      if (x == 0) Water(x -> y)
      else {
        if (x >= 3 && x <= 9 && y >= 7 && y <= 13) {
          val acts =
            if (x == 6 && y == 10) activities(random) ++ Seq(Center)
            else activities(random)

          val level =
            if (acts.contains(Center)) Elite
            else if (acts.contains(Industry)) Poor
            else Middle

          Urban(x -> y, activities = acts, habitatLevel = level)
        } else NotUrban(x -> y)
      }

    val matrix = Vector.tabulate(side, side) {  (i, j) => cell(i, j) }
    val edges = Vector(GenericWay(Vertical, 5), GenericWay(Horizontal, 4))

    Grid(matrix, edges, side)
  }



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


}
