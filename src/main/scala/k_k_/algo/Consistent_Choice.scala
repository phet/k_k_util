/*
   file: k_k_/algo/Consistent_Choice.scala

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
package k_k_.algo {

import java.nio.ByteBuffer
import java.security.MessageDigest

import scala.collection.immutable.MapLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder, MapBuilder}

import k_k_.data.rand._


/**
 *  Alternative `calc_digest` implementation for mixin to
 *  <a href="Consistent_Choice.html">`Consistent_Choice[T]`</a>.
 */
trait SHA_Input_Digest { self: Consistent_Choice[_] =>

  override
  protected final def calc_digest(bytes: Array[Byte]): Long =
    trailing_Long(MessageDigest.getInstance("SHA").digest(bytes))
}


/**
 *  Companion to `Consistent_Choice[T]` with factory methods, and all the defs
 *  and implicits required of a well-heeled
 *  `scala.collection.immutable.Map[_, _]`.
 */
object Consistent_Choice {

  val default_choice_multiple = 60 // divisible by 2, 3, 4, 5, 6, 10, 12, 15...

  val default_digest_name = "MD5"


  def empty[T] = new Consistent_Choice[T]

  /**
   *  Construct a `Consistent_Choice[T]` with the given 'choice multiple'.
   */
  def apply[T](choice_multiple: Int): Consistent_Choice[T] =
    new Consistent_Choice[T](choice_multiple)

  /**
   *  Construct a `Consistent_Choice[T]` with the given choice-weight pairs and
   *  the default 'choice multiple'.
   */
  def apply[T](kvs: (T, Double)*): Consistent_Choice[T] =
    empty[T] ++ kvs

  /*
     this version is too strict in only providing implicit for Pair[_, Double],
     even though one feels it natural to use, e.g. (t: T -> 3); for generality,
     any Pair[_, AnyVal] should work!
 
  def newBuilder[T]: Builder[(T, Double), Consistent_Choice[T]] = 
    new MapBuilder[T, Double, Consistent_Choice[T]](empty)
  
  implicit def canBuildFrom[T]:
      CanBuildFrom[Consistent_Choice[_], (T, Double), Consistent_Choice[T]] = 
    new CanBuildFrom[Consistent_Choice[_], (T, Double), Consistent_Choice[T]] {

      def apply(from: Consistent_Choice[_]) = newBuilder[T]

      def apply() = newBuilder[T]
    }
  */

  def newBuilder[T]: Builder[(T, AnyVal), Consistent_Choice[T]] =
    newBuilder[T](default_choice_multiple)

  def newBuilder[T](choice_multiple: Int):
      Builder[(T, AnyVal), Consistent_Choice[T]] =
    new MapBuilder[T, AnyVal, Consistent_Choice[T]](
                                  new Consistent_Choice[T](choice_multiple)) {
        override def +=(x: (T, AnyVal)) =
          super.+=((x._1, force_double(x._2).getOrElse(0.0)))
    }

  implicit def canBuildFrom[T]:
      CanBuildFrom[Consistent_Choice[_], (T, AnyVal), Consistent_Choice[T]] =
    new CanBuildFrom[Consistent_Choice[_], (T, AnyVal), Consistent_Choice[T]] {

      def apply(from: Consistent_Choice[_]) = newBuilder[T](from.choice_mult)

      def apply() = newBuilder[T]
    }


  private def update_values[T](choice_weights: Map[T, Double],
                               old_choice_values: Map[T, Seq[Long]],
                               choice_multiple: Int):
      Map[T, Seq[Long]] = {
    val obsolete_choices =
      for {
        old_choice <- old_choice_values.keys
        if !choice_weights.isDefinedAt(old_choice)
      } yield old_choice
    val choice_updates =
      for {
        (choice, n_values) <-
            choice_weights.toSeq.map { case (choice, weight) =>
                val n_values = (weight * choice_multiple).toInt
                (choice, n_values)
              }
        val old_values = old_choice_values.get(choice).getOrElse(Seq.empty)
        val values_delta = n_values - old_values.length
        if values_delta != 0
      } yield {
        if (n_values == 0) {
          (choice, None)
        } else {
          val new_values =
            if (values_delta > 0)
              Seq.fill(values_delta)(rand.gen_long) ++ old_values
            else
              old_values.drop(math.abs(values_delta))
          (choice, Some(new_values))
        }
      }
    val mod_choice_values = (old_choice_values /: obsolete_choices) { _ - _ }
    (mod_choice_values /: choice_updates) { case (evolving, (choice, update)) =>
        update match {
          case Some(seq) => evolving + (choice -> seq)
          case None      => evolving - choice
        }
      }
  }

  protected def force_double(v: Any): Option[Double] =
  // protected def force_double(v: AnyVal): Double =
  //
  // (not much in the way of forcing anything, but necessary to return Option
  //  to avert):
  //  error: type AnyVal cannot be used in a type pattern or isInstanceOf test
  //    case v : AnyVal =>
  //      calc_updated(choice_weights + ((kv._1, force_double(v))))
  //  [therefore this function must be defined over AnyRef]
    PartialFunction.condOpt(v) {
      case dbl   : Double  => dbl
      case float : Float   => float.toDouble
      case long  : Long    => long.toDouble
      case int   : Int     => int.toDouble
      case short : Short   => short.toDouble
      case byte  : Byte    => byte.toDouble
      case char  : Char    => char.toDouble
      case bool  : Boolean => if (bool) 1.0 else 0.0
      case unit  : Unit    => 0.0
    }


  private val rand = new Rand_Gen with Truly_Random
}

/**
 *  Consistent hashing / consistent choice scheme for assigning each input
 *  value in a repeatable manner to a choice of some parameterizable type, with
 *  choices probabilistically distributed according to an associated configured
 *  weight.
 *
 *  This data structure is fully immutable and masquerades as a
 *  `scala.collection.immutable.Map[T, Double]` (from choice to weight) to
 *  offer the convenience of `map`, `filter`, `collect`, etc.
 *
 *  It adds the following methods for performing choice:
 *      {{{
 *      def choose_for(bytes: Array[Byte]): Option[T]
 *      def choose_for(str: String): Option[T]
 *      def choose_for(v: AnyVal): Option[T]
 *
 *      def choices_for(bytes: Array[Byte]): Seq[T]
 *      def choices_for(str: String): Seq[T]
 *      def choices_for(v: AnyVal): Seq[T]
 *      }}}
 *
 *  Methods returning `Option[T]` return `None` only when every
 *  `(weight * choice_multiple).toInt == 0`.  Those returning `Seq[T]` return a
 *  lazily-evaluated sequence of every distinct choice ordered according to the
 *  same probabilistic distribution used to select a singular choice.
 *  `Seq.empty` is returned only under the same condition described previously
 *  for `choose_for` and `None`.
 *
 *  An instance may be constructed with an optional 'choice multiple' as a means
 *  of tuning the accuracy of its probability distribution.  This value is
 *  applied to every weight provided.  A larger 'choice multiple' should foster
 *  greater accuracy, but at the expense of memory consumption and, in the case
 *  of `choices_for`, longer time to elaborate each additional value of the lazy
 *  sequence it returns.
 *
 *  The implementation uses a hash digest function internally to map input of
 *  arbitrary length to a `Long` value.  The default is to apply the
 *  relatively-fast <a href="http://en.wikipedia.org/wiki/MD5">MD5</a> digest
 *  algorithm (<a href="http://tools.ietf.org/html/rfc1321">RFC 1321</a>),
 *  and to use only the final 64 of the 128 bits generated.  Those, who would
 *  prefer to use a different hashing scheme, may override the following method:
 *      {{{
 *      protected def calc_digest(bytes: Array[Byte]): Long
 *      }}}
 *
 *  A (useable) example is given within the mixin
 *  <a href="SHA_Input_Digest.html">`SHA_Input_Digest`</a>.
 *
 *  '''Example:'''
 *
 *      {{{
 *      import k_k_.algo.Consistent_Choice
 * 
 *      // load-balance with weight favoring odd numbered handlers 2:1
 *      val handlers = 0 until 5 map { n => ("handler" + n,
 *                                           if (n % 2 == 0) 1.0 else 2.0)
 *          }
 *      // = Seq[(String, Double)](("handler0", 1.0), ("handler1", 2.0), ...)
 *
 *      val choice_multiple = 100
 *      val choices = Consistent_Choice[String](choice_multiple) ++ handlers
 *      ...
 * 
 *      for {
 *        req <- req_stream
 *      } {
 *        val handler = choices.choose_for(calc_req_header_sig(req))
 *        Handler_Mgr.get(handler) match {
 *          case Some(handler_actor) => handler_actor ! req
 *          case None => send_err_response(req)
 *        }
 *      }
 *      }}}
 */
class Consistent_Choice[T] private (choice_weights: Map[T, Double],
                                    choice_values: Map[T, Seq[Long]],
                                    private val choice_mult: Int)
    extends Map[T, Double] with MapLike[T, Double, Consistent_Choice[T]] {

  import Consistent_Choice.{force_double, default_digest_name, update_values}


  def this(choice_multiple: Int = Consistent_Choice.default_choice_multiple) =
    this(Map.empty[T, Double], Map.empty[T, Seq[Long]], choice_multiple)


  /**
   *  Assign input to choice according to configured weighted choices.
   *
   *  Returns `None` iff every `(weight * choice_mult).toInt == 0`
   */
  def choose_for(bytes: Array[Byte]): Option[T] =
    calc_choice(calc_digest(bytes))

  /**
   *  Assign input to choice according to configured weighted choices.
   *
   *  Returns `None` iff every `(weight * choice_mult).toInt == 0`
   */
  def choose_for(str: String): Option[T] =
    choose_for(str.getBytes)

  /**
   *  Assign input to choice according to configured weighted choices.
   *
   *  Returns `None` iff every `(weight * choice_mult).toInt == 0`
   */
  def choose_for(v: AnyVal): Option[T] =
    calc_choice(v.hashCode.toLong)


  /**
   *  Assign input to lazily-elaborated sequence of every distinct choice
   *  ordered according to configured weighted choices.
   *
   *  Returns `Seq.empty` iff every `(weight * choice_mult).toInt == 0`
   */
  def choices_for(bytes: Array[Byte]): Seq[T] =
    calc_choices(calc_digest(bytes))

  /**
   *  Assign input to lazily-elaborated sequence of every distinct choice
   *  ordered according to configured weighted choices.
   *
   *  Returns `Seq.empty` iff every `(weight * choice_mult).toInt == 0`
   */
  def choices_for(str: String): Seq[T] =
    choices_for(str.getBytes)

  /**
   *  Assign input to lazily-elaborated sequence of every distinct choice
   *  ordered according to configured weighted choices.
   *
   *  Returns `Seq.empty` iff every `(weight * choice_mult).toInt == 0`
   */
  def choices_for(v: AnyVal): Seq[T] =
    calc_choices(v.hashCode.toLong)


  def get(choice: T): Option[Double] =
    choice_weights.get(choice)

  def iterator: Iterator[(T, Double)] =
    choice_weights.iterator

  // NOTE: not possible to utilize Numeric[X].toDouble conversion since adding
  // the context bound would defy superclass Map[T, Double]`s contract. (i.e.
  //   def + [N >: Double : Numeric](kv: (T, N)): Consistent_Choice[T] =
  // ... as it is, the contract is broken at run-time, since Map#B (value type)
  // is essentially considered invariant by the extending Consistent_Choice[T].
  // this bad behavior is rationalized by the advantage of telling the compiler
  // that the return type is Consistent_Choice[T], rather than Map[T, X]: one
  // looses the ability to return Map[T, Any], and yet avoids the inconvenience
  // of needing to cast Map[T, AnyVal] to Consistent_Choice[T].
  def + [X >: Double](kv: (T, X)): Consistent_Choice[T] =
    force_double(kv._2) match {
      case Some(v) => calc_updated(choice_weights + (kv._1 -> v))
      case None =>
        throw new IllegalArgumentException("pair value must extend AnyVal")
    }

  def - (key: T): Consistent_Choice[T] =
    calc_updated(choice_weights - key)

  // NOTE: must be overridden (from MapLike[_, _]), since, although, correct
  // covariant return type is returned (at runtime), compiler remains unaware,
  // since MapLike fixes return type as Map[A, B]
  override
  def ++ [X >: Double](xs: TraversableOnce[(T, X)]): Consistent_Choice[T] =
    (this /: xs) { _ + _ }

  override
  def empty = new Consistent_Choice[T](choice_mult)


  /**
   *  Defaults to 'MD5' (RFC 1321); may be overridden, if desired.
   */
  protected def calc_digest(bytes: Array[Byte]): Long =
    trailing_Long(MessageDigest.getInstance(default_digest_name).digest(bytes))

  /**
   *  defined here, rather than companion, for use by mixin overriding
   *  `calc_digest`.
   */
  protected final def trailing_Long(bytes: Array[Byte]): Long = {
    val len = bytes.length
    val safe_bytes =
      if (len < 8) Array.fill[Byte](8 - len)(0) ++ bytes
      else bytes
    ByteBuffer.wrap(bytes).getLong(if (len > 8) len - 9 else 0)
  }


  private def calc_updated(updated_weights: Map[T, Double]) =
    new Consistent_Choice[T](updated_weights,
                             update_values[T](updated_weights, choice_values,
                                              choice_mult),
                             choice_mult)

  private def circular(i: Int): Int = // re-considers index to be 'circular'
    if (i >= 0) i else sorted_index.size - 1

  private def calc_choice(hash: Long): Option[T] = {
    val i = indexOf_leq(hash)
    sorted_index.lift.apply(circular(i)).map( _._2 )
  }

  private def calc_choices(hash: Long): Seq[T] = {
    def get_choice(i: Int): Option[T] =
      sorted_index.lift.apply(circular(i)).map( _._2 )

    val all_possible_choices = choice_values.keySet
    lazy val n_total = all_possible_choices.size

    def stream_choices(i: Int, remaining: collection.Set[T]): Stream[T] =
      if (remaining.isEmpty) Stream.empty
      else get_choice(i) match {
        case None => Stream.empty
        case Some(choice) =>
          if (remaining.contains(choice))
            Stream.cons(choice, stream_choices(i - 1, remaining - choice))
          else if (remaining.size == 1 ||
                   remaining.size.toDouble / n_total < .15) {
            // return (any) one of the remaining to speed up search
            val choice = remaining.head
            Stream.cons(choice, stream_choices(i - 1, remaining - choice))
          } else {
            stream_choices(i - 1, remaining)
          }
      }

    val i = indexOf_leq(hash)
    stream_choices(i, all_possible_choices)
  }

  // WARNING: returns -1 if either v < elem 0 or if sorted_index.size == 0
  private def indexOf_leq(v: Long): Int = {
    def bsearch(beg: Int, end: Int): Int =
      if (end < beg) {
        if (sorted_index.lift.apply(beg).map( _._1 < v ).getOrElse(false)) beg
        else end
      } else {
        val mid = (beg + end) / 2
        val mid_v = sorted_index(mid)._1
        if      (mid_v > v) bsearch(beg, mid - 1)
        else if (mid_v < v) bsearch(mid + 1, end)
        else                mid
      }
    bsearch(0, sorted_index.size - 1)
  }

  // NOTE: `lazy`, so intermediate/ephemeral instances needn't undertake sort
  private lazy val sorted_index =
    (for {
       (choice, values) <- choice_values.toIndexedSeq
       value <- values
     } yield (value, choice)
    ) sorted(
        new Ordering[(Long, T)] {
          def compare (x: (Long, T), y: (Long, T)): Int = {
            val cmp = Ordering.Long.compare(x._1, y._1)
            if (cmp != 0) cmp
            else {
              // NOTE: uncommon for two random Longs to be equal, yet possible
              val (x_id, y_id) = (System.identityHashCode(x._2),
                                  System.identityHashCode(y._2))
              if      (x_id < y_id) -1
              else if (x_id > y_id) 1
              else 0
            }
          }
        })
}

}
