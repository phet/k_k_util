/*
   file: k_k_/data/rand.scala

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
package k_k_.data.rand {

import java.nio.ByteBuffer
import java.security.SecureRandom

import scala.collection.generic.CanBuildFrom
import scala.collection.TraversableOnce

import scala.util.Random


/**
 *  Abstract trait conveying the ability to be seeded by a `Long` value.
 */
trait Seedable {

  protected def seed: Long
}


/**
 *  Implementation of <a href="Seedable.html">`Seedable`</a> that furnishes a
 *  cryptograpically secure seed.
 */
trait Truly_Random { self: Seedable =>

  override
  final protected def seed: Long = {
    val seed_bytes = SecureRandom.getSeed(8)
    ByteBuffer.wrap(seed_bytes).getLong()
  }
}


/**
 *  Implementation of <a href="Seedable.html">`Seedable`</a> that furnishes an
 *  insecure, yet non-repeatable seed.
 */
trait Temporally_Random { self: Seedable =>

  override
  final protected def seed: Long =
    System.currentTimeMillis
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
 *  <a href="Truly_Random.html">`Truly_Random`</a> or
 *  <a href="Temporally_Random.html">`Temporally_Random`</a>
 *
 *  '''Example:'''
 *
 *      {{{
 *      import k_k_.data.rand._
 * 
 *      val rand = new Rand_Gen with Truly_Random // cryptograpically-secure
 *      val (n, guess, blah_blah) = (rand.gen_long,
 *                                   rand.gen_int_between(1, 100),
 *                                   rand.gen_str(5, 25))
 *      }}}
 */
trait Rand_Gen extends Seedable {

  /**
   *  Returns the next randomally-generated `Int`.
   */
  def gen_int: Int =
    rand.nextInt

  /**
   *  Returns the next randomally-generated `Int` between `[ 0, max )`
   */
  def gen_int_below(max: Int): Int =
    rand.nextInt(max)

  /**
   *  Returns the next randomally-generated `Int` between `[ min, max )`
   */
  def gen_int_between(min: Int, max: Int): Int =
    rand.nextInt(max - min) + min

  /**
   *  Returns the next randomally-generated `Long`.
   */
  def gen_long =
    rand.nextLong


  /**
   *  Returns the next randomally-generated `Boolean`.
   */
  def gen_bool =
    rand.nextBoolean


  /**
   *  Returns the next randomally-generated `Double`.
   */
  def gen_double =
    rand.nextDouble

  /**
   *  Returns the next randomally-generated `Float`.
   */
  def gen_float: Float =
    rand.nextFloat


  /**
   *  Returns the next randomally-generated `String` with length `== len`.
   */
  def gen_string(len: Int): String =
    // ridiculous Std. Lib. method returns ('?' * n)--why?!?!?!?
    // rand.nextString(gen_int_between(min_len, max_len))
    Seq.fill(len)(gen_char).mkString("")

  /**
   *  Returns the next randomally-generated `String` with length between
   *  `[ min_len, max_len )`.
   */
  def gen_string(min_len: Int, max_len: Int): String =
    gen_string(gen_int_between(min_len, max_len))


  /**
   *  Returns the next randomally-generated `Char`.
   */
  def gen_char: Char =
    str_chars(rand.nextInt(n_str_chars))

  /**
   *  Returns a new collection of the same type in a randomly shuffled order.
   */
  def shuffle[T, C[X] <: TraversableOnce[X]]
          (xs: C[T])(implicit bf: CanBuildFrom[C[T], T, C[T]]): C[T] =
    rand.shuffle(xs)


  private val rand = new Random(seed)

  private val str_chars = (Seq.empty
                          ++ ('A' to 'Z')
                          ++ ('a' to 'z')
                          ++ ('0' to '9'))

  private val n_str_chars = str_chars.length
}

}
