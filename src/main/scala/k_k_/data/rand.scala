/*
   file: k_k_/data/rand.scala

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
package k_k_.data.rand {

import java.nio.ByteBuffer
import java.security.SecureRandom

import scala.collection.generic.CanBuildFrom
import scala.collection.TraversableOnce

import scala.util.Random


/**
 *  Trait conveying the ability to be seeded by a `Long` value.
 */
trait Seedable {
  protected def seed: Long
}


/**
 *  Implementation of <a href="Seedable.html">`Seedable`</a> that furnishes a
 *  cryptograpically secure seed.
 */
trait TrulyRandom { self: Seedable =>
  override final protected def seed: Long = {
    val seedBytes = SecureRandom.getSeed(8)
    ByteBuffer.wrap(seedBytes).getLong()
  }
}


/**
 *  Implementation of <a href="Seedable.html">`Seedable`</a> that furnishes an
 *  insecure, yet non-repeatable seed.
 */
trait TemporallyRandom { self: Seedable =>
  override final protected def seed: Long = System.currentTimeMillis
}


/**
 *  Abstract <a href="Seedable.html">`Seedable`</a> randomness generator easily
 *  tunable by a mixin policy trait.
 *
 *  To instantiate, it must be associated with an implementation of:
 *     {{{
 *        protected def seed: Long
 *     }}}
 *
 *  For example, via a mixin like
 *  <a href="TrulyRandom.html">`TrulyRandom`</a> or
 *  <a href="TemporallyRandom.html">`TemporallyRandom`</a>
 *
 *  '''Example:'''
 *
 *      {{{
 *      import k_k_.data.rand._
 * 
 *      val rand = new RandGen with TrulyRandom // cryptograpically-secure
 *      val (n, guess, blahBlah) = (rand.genLong,
 *                                   rand.genIntBetween(1, 100),
 *                                   rand.genStr(5, 25))
 *      }}}
 */
trait RandGen extends Seedable {

  /** @return the next randomally-generated `Int`. */
  def genInt: Int = rand.nextInt

  /** @return the next randomally-generated `Int` between `[ 0, max )` */
  def genIntBelow(max: Int): Int = rand.nextInt(max)

  /** @return the next randomally-generated `Int` between `[ min, max )` */
  def genIntBetween(min: Int, max: Int): Int = rand.nextInt(max - min) + min

  /** @return the next randomally-generated `Long`. */
  def genLong = rand.nextLong


  /** @return the next randomally-generated `Boolean`. */
  def genBool = rand.nextBoolean


  /** @return the next randomally-generated `Double`. */
  def genDouble = rand.nextDouble

  /** @return the next randomally-generated `Float`. */
  def genFloat: Float = rand.nextFloat


  /** @return the next randomally-generated `String` with length `== len`. */
  def genString(len: Int): String =
    // ridiculous Std. Lib. method returns ('?' * n)--why?!?!?!?
    // rand.nextString(genIntBetween(minLen, maxLen))
    Seq.fill(len)(genChar).mkString("")

  /** 
   *  @return the next randomally-generated `String` with length between
   *  `[ minLen, maxLen )`.
   */
  def genString(minLen: Int, maxLen: Int): String =
    genString(genIntBetween(minLen, maxLen))


  /** @return the next randomally-generated `Char`. */
  def genChar: Char = strChars(rand.nextInt(nStrChars))

  /** @return a new collection of the same type in a randomly shuffled order. */
  def shuffle[T, C[X] <: TraversableOnce[X]](
      xs: C[T])(implicit bf: CanBuildFrom[C[T], T, C[T]]
    ): C[T] =
    rand.shuffle(xs)


  private val rand = new Random(seed)

  private val strChars = (Seq.empty
                          ++ ('A' to 'Z')
                          ++ ('a' to 'z')
                          ++ ('0' to '9'))

  private val nStrChars = strChars.length
}

}
