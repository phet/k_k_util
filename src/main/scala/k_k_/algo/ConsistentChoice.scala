/*
   file: k_k_/algo/ConsistentChoice.scala

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
package k_k_.algo {

import java.nio.ByteBuffer
import java.security.MessageDigest

import scala.collection.GenTraversableOnce
import scala.collection.immutable.MapLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder, MapBuilder}

import k_k_.data.rand._


/**
 *  Alternative `calcDigest` implementation for mixin to
 *  <a href="ConsistentChoice.html">`ConsistentChoice[T]`</a>.
 */
trait SHAInputDigest[T] { self: ConsistentChoice[T] =>

/*
 * NOTE: in migration from scala 2.9.x to 2.10, needed to add type parameter
 * to avoid:
[ERROR] .../k_k_util/src/main/scala/k_k_/algo/ConsistentChoice.scala:35: error: name clash between inherited members:
[INFO] method ==:(x$1: AnyRef)Boolean in class Object and
[INFO] method ==:(x$1: Any)Boolean in class Any
[INFO] have same type after erasure: (x$1: Object)Boolean
[INFO] trait SHAInputDigest { self: ConsistentChoice[_ <: AnyRef] =>
[INFO]       ^
 */

  override protected final def calcDigest(bytes: Array[Byte]): Long =
    trailing_Long(MessageDigest.getInstance("SHA").digest(bytes))
}


/**
 *  Companion to `ConsistentChoice[T]` with factory methods, and all the defs
 *  and implicits required of a well-heeled
 *  `scala.collection.immutable.Map[_, _]`.
 */
object ConsistentChoice {

  val defaultChoiceMultiple = 60 // divisible by 2, 3, 4, 5, 6, 10, 12, 15...

  val defaultDigestName = "MD5"


  def empty[T] = new ConsistentChoice[T]

  /**
   *  Construct a `ConsistentChoice[T]` with the given 'choice multiple'.
   */
  def apply[T](choiceMultiple: Int): ConsistentChoice[T] =
    new ConsistentChoice[T](choiceMultiple)

  /**
   *  Construct a `ConsistentChoice[T]` with the given choice-weight pairs and
   *  the default 'choice multiple'.
   */
  def apply[T](kvs: (T, Double)*): ConsistentChoice[T] =
    empty[T] ++ kvs

  /*
     this version is too strict in only providing implicit for Pair[_, Double],
     even though one feels it natural to use, e.g. (t: T -> 3); for generality,
     any Pair[_, AnyVal] should work!
 
  def newBuilder[T]: Builder[(T, Double), ConsistentChoice[T]] = 
    new MapBuilder[T, Double, ConsistentChoice[T]](empty)
  
  implicit def canBuildFrom[T]:
      CanBuildFrom[ConsistentChoice[_], (T, Double), ConsistentChoice[T]] = 
    new CanBuildFrom[ConsistentChoice[_], (T, Double), ConsistentChoice[T]] {

      def apply(from: ConsistentChoice[_]) = newBuilder[T]

      def apply() = newBuilder[T]
    }
  */

  def newBuilder[T]: Builder[(T, AnyVal), ConsistentChoice[T]] =
    newBuilder[T](defaultChoiceMultiple)

  def newBuilder[T](choiceMultiple: Int):
      Builder[(T, AnyVal), ConsistentChoice[T]] =
    new MapBuilder[T, AnyVal, ConsistentChoice[T]](
        new ConsistentChoice[T](choiceMultiple)
      ) {
      override def +=(x: (T, AnyVal)) =
        super.+=((x._1, forceDouble(x._2).getOrElse(0.0)))
    }

  implicit def canBuildFrom[T]:
      CanBuildFrom[ConsistentChoice[_], (T, AnyVal), ConsistentChoice[T]] =
    new CanBuildFrom[ConsistentChoice[_], (T, AnyVal), ConsistentChoice[T]] {
      def apply(from: ConsistentChoice[_]) = newBuilder[T](from.choiceMult)
      def apply() = newBuilder[T]
    }


  private def updateValues[T](
      choiceWeights: Map[T, Double],
      oldChoiceValues: Map[T, Seq[Long]],
      choiceMultiple: Int
    ): Map[T, Seq[Long]] = {
    val obsoleteChoices =
      for {
        oldChoice <- oldChoiceValues.keys
        if !choiceWeights.isDefinedAt(oldChoice)
      } yield oldChoice
    val choiceUpdates =
      for {
        (choice, nValues) <- choiceWeights.toSeq.map { case (choice, weight) =>
          val nValues = (weight * choiceMultiple).toInt
          (choice, nValues)
        }
        oldValues = oldChoiceValues.get(choice).getOrElse(Seq.empty)
        valuesDelta = nValues - oldValues.length
        if valuesDelta != 0
      } yield {
        if (nValues == 0) {
          (choice, None)
        } else {
          val newValues =
            if (valuesDelta > 0)
              Seq.fill(valuesDelta)(rand.genLong) ++ oldValues
            else
              oldValues.drop(math.abs(valuesDelta))
          (choice, Some(newValues))
        }
      }
    val modChoiceValues = (oldChoiceValues /: obsoleteChoices) { _ - _ }
    (modChoiceValues /: choiceUpdates) { case (evolving, (choice, update)) =>
      update match {
        case Some(seq) => evolving + (choice -> seq)
        case None      => evolving - choice
      }
    }
  }

  protected def forceDouble(v: Any): Option[Double] =
  // protected def forceDouble(v: AnyVal): Double =
  //
  // (not much in the way of forcing anything, but necessary to return Option
  //  to avert):
  //  error: type AnyVal cannot be used in a type pattern or isInstanceOf test
  //    case v : AnyVal =>
  //      calcUpdated(choiceWeights + ((kv._1, forceDouble(v))))
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


  private val rand = new RandGen with TrulyRandom
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
 *      def chooseFor(bytes: Array[Byte]): Option[T]
 *      def chooseFor(str: String): Option[T]
 *      def chooseFor(v: AnyVal): Option[T]
 *
 *      def choicesFor(bytes: Array[Byte]): Seq[T]
 *      def choicesFor(str: String): Seq[T]
 *      def choicesFor(v: AnyVal): Seq[T]
 *      }}}
 *
 *  Methods returning `Option[T]` return `None` only when every
 *  `(weight * choiceMultiple).toInt == 0`.  Those returning `Seq[T]` return a
 *  lazily-evaluated sequence of every distinct choice ordered according to the
 *  same probabilistic distribution used to select a singular choice.
 *  `Seq.empty` is returned only under the same condition described previously
 *  for `chooseFor` and `None`.
 *
 *  An instance may be constructed with an optional 'choice multiple' as a means
 *  of tuning the accuracy of its probability distribution.  This value is
 *  applied to every weight provided.  A larger 'choice multiple' should foster
 *  greater accuracy, but at the expense of memory consumption and, in the case
 *  of `choicesFor`, longer time to elaborate each additional value of the lazy
 *  sequence it returns.
 *
 *  The implementation uses a hash digest function internally to map input of
 *  arbitrary length to a `Long` value.  The default is to apply the
 *  relatively-fast <a href="http://en.wikipedia.org/wiki/MD5">MD5</a> digest
 *  algorithm (<a href="http://tools.ietf.org/html/rfc1321">RFC 1321</a>),
 *  and to use only the final 64 of the 128 bits generated.  Those, who would
 *  prefer to use a different hashing scheme, may override the following method:
 *      {{{
 *      protected def calcDigest(bytes: Array[Byte]): Long
 *      }}}
 *
 *  A (useable) example is given within the mixin
 *  <a href="SHAInputDigest.html">`SHAInputDigest`</a>.
 *
 *  '''Example:'''
 *
 *      {{{
 *      import k_k_.algo.ConsistentChoice
 * 
 *      // load-balance with weight favoring odd numbered handlers 2:1
 *      val handlers = 0 until 5 map { n => ("handler" + n,
 *                                           if (n % 2 == 0) 1.0 else 2.0)
 *          }
 *      // = Seq[(String, Double)](("handler0", 1.0), ("handler1", 2.0), ...)
 *
 *      val choiceMultiple = 100
 *      val choices = ConsistentChoice[String](choiceMultiple) ++ handlers
 *      ...
 * 
 *      for {
 *        req <- reqStream
 *      } {
 *        val handler = choices.chooseFor(calcReqHeaderSig(req))
 *        HandlerMgr.get(handler) match {
 *          case Some(handlerActor) => handlerActor ! req
 *          case None => sendErrResponse(req)
 *        }
 *      }
 *      }}}
 */
class ConsistentChoice[T] private (
    choiceWeights: Map[T, Double],
    choiceValues: Map[T, Seq[Long]],
    private val choiceMult: Int
  ) extends Map[T, Double] with MapLike[T, Double, ConsistentChoice[T]] {

  import ConsistentChoice.{forceDouble, defaultDigestName, updateValues}


  def this(choiceMultiple: Int = ConsistentChoice.defaultChoiceMultiple) =
    this(Map.empty[T, Double], Map.empty[T, Seq[Long]], choiceMultiple)


  /**
   *  Assign input to choice according to configured weighted choices.
   *
   *  @return `None` iff every `(weight * choiceMult).toInt == 0`
   */
  def chooseFor(bytes: Array[Byte]): Option[T] = calcChoice(calcDigest(bytes))

  /**
   *  Assign input to choice according to configured weighted choices.
   *
   *  @return `None` iff every `(weight * choiceMult).toInt == 0`
   */
  def chooseFor(str: String): Option[T] = chooseFor(str.getBytes)

  /**
   *  Assign input to choice according to configured weighted choices.
   *
   *  @return `None` iff every `(weight * choiceMult).toInt == 0`
   */
  def chooseFor(v: AnyVal): Option[T] = calcChoice(v.hashCode.toLong)


  /**
   *  Assign input to lazily-elaborated sequence of every distinct choice
   *  ordered according to configured weighted choices.
   *
   *  @return `Seq.empty` iff every `(weight * choiceMult).toInt == 0`
   */
  def choicesFor(bytes: Array[Byte]): Seq[T] = calcChoices(calcDigest(bytes))

  /**
   *  Assign input to lazily-elaborated sequence of every distinct choice
   *  ordered according to configured weighted choices.
   *
   *  @return `Seq.empty` iff every `(weight * choiceMult).toInt == 0`
   */
  def choicesFor(str: String): Seq[T] = choicesFor(str.getBytes)

  /**
   *  Assign input to lazily-elaborated sequence of every distinct choice
   *  ordered according to configured weighted choices.
   *
   *  @return `Seq.empty` iff every `(weight * choiceMult).toInt == 0`
   */
  def choicesFor(v: AnyVal): Seq[T] = calcChoices(v.hashCode.toLong)


  def get(choice: T): Option[Double] = choiceWeights.get(choice)

  def iterator: Iterator[(T, Double)] = choiceWeights.iterator

  // NOTE: not possible to utilize Numeric[X].toDouble conversion since adding
  // the context bound would defy superclass Map[T, Double]`s contract. (i.e.
  //   def + [N >: Double : Numeric](kv: (T, N)): ConsistentChoice[T] =
  // ... as it is, the contract is broken at run-time, since Map#B (value type)
  // is essentially considered invariant by the extending ConsistentChoice[T].
  // this bad behavior is rationalized by the advantage of telling the compiler
  // that the return type is ConsistentChoice[T], rather than Map[T, X]: one
  // looses the ability to return Map[T, Any], and yet avoids the inconvenience
  // of needing to cast Map[T, AnyVal] to ConsistentChoice[T].
  def + [X >: Double](kv: (T, X)): ConsistentChoice[T] =
    forceDouble(kv._2) match {
      case Some(v) => calcUpdated(choiceWeights + (kv._1 -> v))
      case None =>
        throw new IllegalArgumentException("pair value must extend AnyVal")
    }

  def - (key: T): ConsistentChoice[T] = calcUpdated(choiceWeights - key)

  // NOTE: must be overridden (from MapLike[_, _]), since, although, correct
  // covariant return type is returned (at runtime), compiler remains unaware,
  // since MapLike fixes return type as Map[A, B]
  override def ++ [X >: Double](xs: GenTraversableOnce[(T, X)]):
      ConsistentChoice[T] =
    (this /: xs) { _ + _ }

  override def empty = new ConsistentChoice[T](choiceMult)


  /**
   *  Defaults to 'MD5' (RFC 1321); may be overridden, if desired.
   */
  protected def calcDigest(bytes: Array[Byte]): Long =
    trailing_Long(MessageDigest.getInstance(defaultDigestName).digest(bytes))

  /**
   *  defined here, rather than companion, for use by mixin overriding
   *  `calcDigest`.
   */
  protected final def trailing_Long(bytes: Array[Byte]): Long = {
    val len = bytes.length
    val safeBytes =
      if (len < 8) Array.fill[Byte](8 - len)(0) ++ bytes
      else bytes
    ByteBuffer.wrap(bytes).getLong(if (len > 8) len - 9 else 0)
  }


  private def calcUpdated(updatedWeights: Map[T, Double]) =
    new ConsistentChoice[T](
        updatedWeights,
        updateValues[T](updatedWeights, choiceValues, choiceMult),
        choiceMult
      )

  private def circular(i: Int): Int = // re-considers index to be 'circular'
    if (i >= 0) i else sortedIndex.size - 1

  private def calcChoice(hash: Long): Option[T] = {
    val i = indexOfLteq(hash)
    sortedIndex.lift.apply(circular(i)).map( _._2 )
  }

  private def calcChoices(hash: Long): Seq[T] = {
    def getChoice(i: Int): Option[T] =
      sortedIndex.lift.apply(circular(i)).map( _._2 )

    val allPossibleChoices = choiceValues.keySet
    lazy val nTotal = allPossibleChoices.size

    def streamChoices(i: Int, remaining: collection.Set[T]): Stream[T] =
      if (remaining.isEmpty) Stream.empty
      else getChoice(i) match {
        case None => Stream.empty
        case Some(choice) =>
          if (remaining.contains(choice))
            Stream.cons(choice, streamChoices(i - 1, remaining - choice))
          else if (remaining.size == 1 ||
                   remaining.size.toDouble / nTotal < .15) {
            // return (any) one of the remaining to speed up search
            val choice = remaining.head
            Stream.cons(choice, streamChoices(i - 1, remaining - choice))
          } else {
            streamChoices(i - 1, remaining)
          }
      }

    val i = indexOfLteq(hash)
    streamChoices(i, allPossibleChoices)
  }

  // WARNING: returns -1 if either v < elem 0 or if sortedIndex.size == 0
  private def indexOfLteq(v: Long): Int = {
    def bsearch(beg: Int, end: Int): Int =
      if (end < beg) {
        if (sortedIndex.lift.apply(beg).map( _._1 < v ).getOrElse(false)) beg
        else end
      } else {
        val mid = (beg + end) / 2
        val midV = sortedIndex(mid)._1
        if      (midV > v) bsearch(beg, mid - 1)
        else if (midV < v) bsearch(mid + 1, end)
        else                mid
      }
    bsearch(0, sortedIndex.size - 1)
  }

  // NOTE: `lazy`, so intermediate/ephemeral instances needn't undertake sort
  private lazy val sortedIndex =
    (for {
       (choice, values) <- choiceValues.toIndexedSeq
       value <- values
     } yield (value, choice)
    ) sorted(
        new Ordering[(Long, T)] {
          def compare (x: (Long, T), y: (Long, T)): Int = {
            val cmp = Ordering.Long.compare(x._1, y._1)
            if (cmp != 0) cmp
            else {
              // NOTE: uncommon for two random Longs to be equal, yet possible
              val (xId, yId) = (System.identityHashCode(x._2),
                                  System.identityHashCode(y._2))
              if      (xId < yId) -1
              else if (xId > yId) 1
              else 0
            }
          }
        }
      )
}

}
