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

import scala.annotation.tailrec
import scala.util.Random

import cell._

object rule {

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
          (Grid.lens(u) composePrism
            Urban.prism composeLens
            Urban.habitatLevel modify HabitatLevel.downgrade) (g)
        else g
      }
    }
  }

  def upgradeHabitations(p: Double) = new Rule {
    override def apply(grid: Grid, rng: Random): Grid = {
      Grid.cells(grid).collect { case x: Urban => x }.foldLeft(grid) { (g, u) =>
        if (u.activities.forall(_ != Industry) && rng.nextDouble() < p)
          (Grid.lens(u) composePrism
            Urban.prism composeLens
            Urban.habitatLevel modify HabitatLevel.upgrade)(g)
        else g
      }
    }
  }


  trait Rule extends ((Grid, Random) => Grid)


}
