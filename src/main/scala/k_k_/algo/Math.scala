/*
   file: k_k_/algo/Math.scala

   Copyright (c) 2011 Corbin "Kip" Kohn

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package k_k_.algo


/**
 *  Supplementary functionality to the `scala.math` package.
 */
object Math {

  /**
   *  Returns `(x, y)` iff `x >= y`; else `(y, x)`
   */
  def max_min[T](x: T, y: T)(implicit cmp: Ordering[T]): (T, T) =
    (x, y) match {
      case (max, min) if (cmp.gteq(max, min)) => (max, min)
      case (min, max)                         => (max, min)
    }

  /**
   *  Returns `(x, y)` iff `x <= y`; else `(y, x)`
   */
  def min_max[T](x: T, y: T)(implicit cmp: Ordering[T]): (T, T) =
    (x, y) match {
      case (min, max) if (cmp.lteq(min, max)) => (min, max)
      case (max, min)                         => (min, max)
    }
}
