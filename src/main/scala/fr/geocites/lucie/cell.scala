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

import monocle.macros.Lenses
import activity._

object cell {

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

    def prism = monocle.Prism[Cell, Urban] {
      case u: Urban => Some(u)
      case _ => None
    } (identity)

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

  sealed trait HabitatLevel
  case object Elite extends HabitatLevel
  case object Middle extends HabitatLevel
  case object Poor extends HabitatLevel
}


