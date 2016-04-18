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

import fr.geocite.lucie.data._
import cell._

object grid {

  type Centrality = Grid => PartialFunction[Location, Double]

  object Grid {
    def neighbourCells(grid: Grid, l: Location, size: Int) =
      neighbours(grid.side, l, size).map {
        case (i, j) => grid.cells(i)(j)
      }

    /* définition d'un voisinage*/
    def neighbours(side: Int, location: Location, size: Int) = {
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

    def cellLenses(g: Grid) = coordinates(g.side).map { case (i, j) => lens(i, j) }

    def cells(g: Grid) = g.cells.flatten


    def lens(cell: Cell): monocle.Lens[Grid, Cell] = lens(cell.location)

    /* Renvoie un couple set / get qui remplace / renvoie un cell particuliére dans une grille */
    def lens(location: Location): monocle.Lens[Grid, Cell] = {
      val (x, y) = location
      monocle.Lens { (g: Grid) => g.cells(x)(y) } {
        c => g =>
          val line = g.cells(x)
          val newCells = g.cells.updated(x, line.updated(y, c))
          g.copy(cells = newCells)
      }
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

}
