/*
   file: k_k_/value.scala

   Copyright (c) 2011-2013 Corbin "Kip" Kohn

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

  /** @return `value`, perhaps modified to be within `[ minValue, maxValue ]` */
  def apply[T](value: T, minValue: T, maxValue: T)(implicit cmp: Ordering[T]):
      T =
    if      (cmp.lt(value, minValue)) minValue
    else if (cmp.lt(maxValue, value)) maxValue
    else value

  /** @return `Some(value)` iff within `[ minValue, maxValue ]`; else `None` */
  def ?[T](value: T, minValue: T, maxValue: T)(implicit cmp: Ordering[T]):
      Option[T] =
    if      (cmp.lt(value, minValue)) None
    else if (cmp.lt(maxValue, value)) None
    else                               Some(value)


  /** @return `value`, perhaps modified to be less than `maxValue` */
  def <[T](value: T, maxValue: T)(implicit cmp: Ordering[T]): T =
    cmpOp( cmp.lt _ )(value, maxValue)

  // currying-style seems less clearly expressed, and appears to run into syntax
  // issues, due to implicit:
  // Clamp.<(a, b) is too many args, and Clamp.<()(a, b) gives too few args err!
  //  def <[T](implicit cmp: Ordering[T]): (T, T) => T =
  //    cmpOp( cmp.lt _ ) _

  /** @return `value` perhaps modified to be less than or equal to `maxValue` */
  def <=[T](value: T, maxValue: T)(implicit cmp: Ordering[T]): T =
    cmpOp( cmp.lteq _ )(value, maxValue)

  /** @return `value`, perhaps modified to be greater than `minValue` */
  def >[T](value: T, minValue: T)(implicit cmp: Ordering[T]): T =
    cmpOp( cmp.gt _ )(value, minValue)

  /** @return `value`, perhaps modified to be greater or equal to `minValue` */
  def >=[T](value: T, minValue: T)(implicit cmp: Ordering[T]): T =
    cmpOp( cmp.gteq _ )(value, minValue)


  /** @return `Some(value)` iff less than `maxValue`; else `None` */
  def ?<[T](value: T, maxValue: T)(implicit cmp: Ordering[T]): Option[T] =
    cmpOpOpt( cmp.lt _ )(value, maxValue)

  /** @return `Some(value)` iff less than or equal to `maxValue`; else `None` */
  def ?<=[T](value: T, maxValue: T)(implicit cmp: Ordering[T]): Option[T] =
    cmpOpOpt( cmp.lteq _ )(value, maxValue)

  /** @return `Some(value)` iff greater than `minValue`; else `None` */
  def ?>[T](value: T, minValue: T)(implicit cmp: Ordering[T]): Option[T] =
    cmpOpOpt( cmp.gt _ )(value, minValue)

  /** @return `Some(value)` iff greater or equal to `minValue`; else `None` */
  def ?>=[T](value: T, minValue: T)(implicit cmp: Ordering[T]): Option[T] =
    cmpOpOpt( cmp.gteq _ )(value, minValue)


  private def cmpOp[T](op: (T, T) => Boolean)(value: T, opValue: T): T =
    if (op(value, opValue)) value
    else                    opValue

  private def cmpOpOpt[T](op: (T, T) => Boolean)(value: T, opValue: T):
      Option[T] =
    if (op(value, opValue)) Some(value)
    else                    None
}
