
/*
 * Copyright (C) 21/07/14 mathieu
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package client

import org.scalajs.dom.raw.{SVGElement, Node}

import scalatags.JsDom._
import scala.util.{Failure, Success}
import all._
import rx._
import org.scalajs.dom
import org.scalajs.dom.Element
import scalatags.JsDom.{svgTags => svgTags}


/**
 * A minimal binding between Scala.Rx and Scalatags and Scala-Js-Dom
 */
object JsRxTags {

  /**
   * Wraps reactive strings in spans, so they can be referenced/replaced
   * when the Rx changes.
   */
  implicit def RxStr[T](r: Rx[T])(implicit f: T => Modifier): Modifier = {
    rxHTMLMod(Rx(span(r())))
  }

  /**
   * Sticks some Rx into a Scalatags fragment, which means hooking up an Obs
   * to propagate changes into the DOM via the element's ID. Monkey-patches
   * the Obs onto the element itself so we have a reference to kill it when
   * the element leaves the DOM (e.g. it gets deleted).
   */
  implicit def rxHTMLMod[T <: dom.raw.HTMLElement](r: Rx[HtmlTag]): Modifier = bindNode(rxHTMLNode(r))


  implicit def rxHTMLNode[T <: dom.raw.HTMLElement](r: Rx[HtmlTag]): Node = {
    def rSafe = r.toTry match {
      case Success(v) => v.render
      case Failure(e) => span(e.toString, backgroundColor := "red").render
    }
    var last = rSafe
    Obs(r, skipInitial = true) {
      val newLast = rSafe
      last.parentElement.replaceChild(newLast, last)
      last = newLast
    }
    last
  }


  /**
    * Idem for SVG elements
    */
  implicit def rxSVGMod[T <: TypedTag[SVGElement]](r: Rx[T]): Modifier = {
    def rSafe = r.toTry match {
      case Success(v) => v.render
      case Failure(e) => span(e.toString, backgroundColor := "red").render
    }
    var last = rSafe
    Obs(r, skipInitial = true) {
      val newLast = rSafe
      last.parentNode.replaceChild(newLast, last)
      last = newLast
    }
    last
  }

  implicit def RxAttrValue[T: scalatags.JsDom.AttrValue] = new scalatags.JsDom.AttrValue[Rx[T]] {
    def apply(t: Element, a: Attr, r: Rx[T]): Unit = {
      Obs(r) {
        implicitly[scalatags.JsDom.AttrValue[T]].apply(t, a, r())
      }
    }
  }

  implicit def RxStyleValue[T: scalatags.JsDom.StyleValue] = new scalatags.JsDom.StyleValue[Rx[T]] {
    def apply(t: Element, s: Style, r: Rx[T]): Unit = {
      Obs(r) {
        implicitly[scalatags.JsDom.StyleValue[T]].apply(t, s, r())
      }
    }
  }

}
