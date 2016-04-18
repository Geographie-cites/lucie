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

import fr.geocites.lucie.data._
import cell._

object grid {

  type Centrality = Grid => PartialFunction[Location, Double]

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

  def cellLenses(g: Grid) = coordinates(g.side).map { case (i, j) => cellLens(i, j) }

  def cells(g: Grid) = g.cells.flatten

  def cellLens(c: Cell): monocle.Lens[Grid, Cell] = cellLens(c.location)

  /* Renvoie un couple set / get qui remplace / renvoie un cell particuliére dans une grille */
  def cellLens(location: Location): monocle.Lens[Grid, Cell] = {
    val (x, y) = location
    monocle.Lens { (g: Grid) => g.cells(x)(y) } {
      c => g =>
        val line = g.cells(x)
        val newCells = g.cells.updated(x, line.updated(y, c))
        g.copy(cells = newCells)
    }
  }

  object Edge {
    def toCSV(edge: GenericWay) =
      s"${edge.orientation},${edge.coordinate}"
  }

}
