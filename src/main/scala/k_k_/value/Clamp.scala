/*
   file: k_k_/value.scala

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
package k_k_.value


/**
 *  Functions for constraining the range of a value.
 */
object Clamp {

  /**
   *  Returns `value`, if need be, massaged to be within
   *  `[ min_value, max_value ]`
   */
  def apply[T](value: T, min_value: T, max_value: T)
              (implicit cmp: Ordering[T]): T =
    if      (cmp.lt(value, min_value)) min_value
    else if (cmp.lt(max_value, value)) max_value
    else value

  /**
   *  Returns `Some(value)` iff within `[ min_value, max_value ]`; else `None`
   */
  def ?[T](value: T, min_value: T, max_value: T)
          (implicit cmp: Ordering[T]): Option[T] =
    if      (cmp.lt(value, min_value)) None
    else if (cmp.lt(max_value, value)) None
    else                               Some(value)


  /**
   *  Returns `value`, if need be, massaged to be less than `max_value`
   */
  def <[T](value: T, max_value: T)(implicit cmp: Ordering[T]): T =
    cmp_op( cmp.lt _ )(value, max_value)

  // currying-style seems less clearly expressed, and appears to run into syntax
  // issues, due to implicit:
  // Clamp.<(a, b) is too many args, and Clamp.<()(a, b) gives too few args err!
  //  def <[T](implicit cmp: Ordering[T]): (T, T) => T =
  //    cmp_op( cmp.lt _ ) _

  /**
   *  Returns `value`, if need be, massaged to be less than or equal to
   *  `max_value`
   */
  def <=[T](value: T, max_value: T)(implicit cmp: Ordering[T]): T =
    cmp_op( cmp.lteq _ )(value, max_value)

  /**
   *  Returns `value`, if need be, massaged to be greater than `min_value`
   */
  def >[T](value: T, min_value: T)(implicit cmp: Ordering[T]): T =
    cmp_op( cmp.gt _ )(value, min_value)

  /**
   *  Returns `value`, if need be, massaged to be greater than or equal to
   *  `min_value`
   */
  def >=[T](value: T, min_value: T)(implicit cmp: Ordering[T]): T =
    cmp_op( cmp.gteq _ )(value, min_value)


  /**
   *  Returns `Some(value)` iff less than `max_value`; else `None`
   */
  def ?<[T](value: T, max_value: T)(implicit cmp: Ordering[T]): Option[T] =
    cmp_op_opt( cmp.lt _ )(value, max_value)

  /**
   *  Returns `Some(value)` iff less than or equal to `max_value`; else `None`
   */
  def ?<=[T](value: T, max_value: T)(implicit cmp: Ordering[T]): Option[T] =
    cmp_op_opt( cmp.lteq _ )(value, max_value)

  /**
   *  Returns `Some(value)` iff greater than `min_value`; else `None`
   */
  def ?>[T](value: T, min_value: T)(implicit cmp: Ordering[T]): Option[T] =
    cmp_op_opt( cmp.gt _ )(value, min_value)

  /**
   * Returns `Some(value)` iff greater than or equal to `min_value`; else `None`
   */
  def ?>=[T](value: T, min_value: T)(implicit cmp: Ordering[T]): Option[T] =
    cmp_op_opt( cmp.gteq _ )(value, min_value)


  private def cmp_op[T](op: (T, T) => Boolean)(value: T, op_value: T): T =
    if (op(value, op_value)) value
    else                     op_value

  private def cmp_op_opt[T](op: (T, T) => Boolean)
                           (value: T, op_value: T): Option[T] =
    if (op(value, op_value)) Some(value)
    else                     None
}
