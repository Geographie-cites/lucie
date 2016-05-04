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

import fr.geocites.lucie.data._
import fr.geocites.lucie.export._
import fr.geocites.lucie.rule._

object dynamic {

  def simulate(state: State, rule: Rule, steps: Int, logger: Logger = Logger.empty) = {
    def simulate0(currentStep: Int, state: State): Grid = {
      logger(Event.Step(currentStep, state.grid))
      if(currentStep >= steps) state.grid
      else {
        val newGrid = rule(state)
        simulate0(currentStep + 1, newGrid)
      }
    }

    simulate0(0, state)
  }


}
