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
import fr.geocites.lucie.data._
import grid._
import monocle.macros.Lenses

object rule {

  @Lenses case class State(grid: Grid, random: Random)
  type Rule = State => State

  /**
    * Choose a rule at random between several rules
    *
    * @param rules set of (rule, weight)
    */
  def multinomialChoice(rules: (Rule, Double)*): Rule =
    (s: State) => multinomial[Rule](rules: _*)(s.random).apply(s)

  /**
    *  Move activity from an urban cell to another
    *
    * @param activity activity to move
    * @param centrality centrality function
    */
  def urbanToUrbanRandomMove(activity: Activity, centrality: Centrality): Rule =
    (s: State) => {
      val gridCentrality = centrality(s.grid)

      val urbanOrigin = randomCellWithActivity(s.grid, activity, s.random, gridCentrality)

      /* Création d'une liste de cells URBAN de destination possibles */
      val destinations =
        cells(s.grid).
          filter(c => !hasSameLocation(c, urbanOrigin)).
          collect { case (x: Urban) => x }.map { urb =>
          val weight =  1 - gridCentrality(urb.location)
          (urb, weight)
        }

      /* Choose the destination at random given the weights */
      val urbanDestination = multinomial(destinations: _*)(s.random)

      /* Update thegrid by setting origin and destination with updated cells */

      def updateGrid =
        (cellLens(urbanOrigin).set(removeActivity(urbanOrigin, activity)) andThen
          cellLens(urbanDestination).set(addActivity(urbanDestination, activity)))

      State.grid.modify(updateGrid)(s)
    }


  /**
    * Move activity from a urban cell to a peripheral non-urban cell
    * It transform the destination cell into a urban cell
    *
    * @param activity
    * @param wayAttractivity attractivity of the transportation ways
    * @param peripheralNeigborhoudSize
    * @param centrality centrality function
    * @param buildUrbanCell
    * @return
    */
  def urbanToNotUrbanRandomMove(
    activity: Activity,
    wayAttractivity: Double,
    peripheralNeigborhoudSize: Int,
    centrality: Centrality,
    buildUrbanCell: (Location, Activity) => Urban): Rule =
    (s: State) => {
      val gridCentrality = centrality(s.grid)
      val urbanOrigin = randomCellWithActivity(s.grid, activity, s.random, gridCentrality)

      /* Select the destination uniformly at random */
      val attractivityMatrix =
        cells(s.grid).map { c =>
          val (x, y) = c.location
          c -> (aggregatedAttractivity(s.grid, peripheralNeigborhoudSize, wayAttractivity)(x, y))
        }.collect { case x@(_: NotUrban, _) => x }

      if(attractivityMatrix.isEmpty) s
      else {
        val destination = multinomial(attractivityMatrix: _*)(s.random)
        def updateGrid =
          cellLens(urbanOrigin).set(removeActivity(urbanOrigin, activity)) andThen
           (cellLens(destination).set(buildUrbanCell(destination.location, activity)))

        State.grid.modify(updateGrid) (s)
      }
    }


  /**
    * Decease the level of habitation near an industry
    */
  def downgradeNearIndustryHabitations(p: Double): Rule =
    (s: State) => {
      def newGrid =
        cells(s.grid).collect { case x: Urban => x }.foldLeft(s.grid) { (g, u) =>
          if (u.activities.exists(_ == Industry) && s.random.nextDouble() < p)
            (cellLens(u) composePrism
              urbanPrism composeLens
              Urban.habitatLevel modify downgrade) (g)
          else g
        }
      State.grid.set(newGrid)(s)
    }

  /**
    * Increase the level of habitation near an industry
    */
  def upgradeHabitations(p: Double): Rule =
    (s: State) => {
      def newGrid =
        cells(s.grid).collect { case x: Urban => x }.foldLeft(s.grid) { (g, u) =>
          if (u.activities.forall(_ != Industry) && s.random.nextDouble() < p)
            (cellLens(u) composePrism
              urbanPrism composeLens
              Urban.habitatLevel modify upgrade)(g)
          else g
        }
      State.grid.set(newGrid)(s)
    }

  /* Helper functions */

  def peripheralAttractivity(grid: Grid, peripheralNeigborhoudSize: Int)(x: Int, y: Int): Double =
    grid.cells(x)(y) match {
      case _: NotUrban =>
        def peripheral =
          neighbourCells(grid, x -> y, peripheralNeigborhoudSize).exists {
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


  def multinomial[T](values: (T, Double)*)(implicit random: Random): T = {
     @tailrec def multinomial0[T](values: List[(T, Double)])(draw: Double): T = {
      values match {
        case Nil ⇒ throw new RuntimeException("List should never be empty.")
        case (bs, _) :: Nil ⇒ bs
        case (bs, weight) :: tail ⇒
          if (draw <= weight) bs
          else multinomial0(tail)(draw - weight)
      }
    }

    val max =  values.map(_._2).sum
    val drawn = random.nextDouble() * max

    multinomial0(values.toList)(drawn)
  }
  
  def randomCellWithActivity(grid: Grid, activity: Activity, random: Random, centrality: PartialFunction[Location, Double]) = {
    val selectedCells =
      cells(grid).collect { case (x: Urban) => x }.flatMap { urban =>
        if(urban.activities.contains(activity)) Some(urban -> centrality(urban.location))
        else None
      }

    multinomial(selectedCells: _*)(random)
  }










}
