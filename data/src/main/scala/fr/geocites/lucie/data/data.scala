/**
  * Created by Romain Reuillon on 22/03/16.
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

import monocle.macros._

package object data {
  sealed trait Activity
  case object Industry extends Activity
  case object Center extends Activity

  type Location = (Int, Int)


  sealed trait Cell {
    def location: Location
  }

  // TODO add flow direction
  case class Water(location: Location) extends Cell
  /**
    * Urban cell
    *
    * @param activities list of activities of the urban cell
    */
  @Lenses case class Urban(location: Location, activities: Vector[Activity], habitatLevel: HabitatLevel) extends Cell
  case class NotUrban(location: Location) extends Cell

  sealed trait HabitatLevel
  case object Elite extends HabitatLevel
  case object Middle extends HabitatLevel
  case object Poor extends HabitatLevel

  /* Définition d'une classe Grid, composé de vecteurs, de edges et de side*/
  case class Grid(cells: Vector[Vector[Cell]], ways: Vector[GenericWay], side: Int)

  sealed trait Orientation
  case object Horizontal extends Orientation
  case object Vertical extends Orientation


  case class GenericWay(orientation: Orientation, coordinate: Int)

}
