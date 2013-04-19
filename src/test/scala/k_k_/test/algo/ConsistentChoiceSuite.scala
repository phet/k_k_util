/*
   file: k_k_/test/algo/ConsistentChoiceSuite.scala

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
package k_k_.test.algo

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.scalatest.matchers.{ShouldMatchers, BeMatcher, MatchResult}

import java.net.URL
import java.util.concurrent.{CountDownLatch, Executors}
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConversions._
import scala.collection.mutable.ConcurrentMap

import k_k_.data.rand._

import k_k_.algo.ConsistentChoice


// NOTE: since type class bound ClassManifest (not Manifest), unable to verify
// type ctor params (i.e. HOType[_] verifiable, but not ArgT in HOType[ArgT])
class IsInstanceOfMatcher[T: ClassManifest] extends BeMatcher[Any] {
  def apply(x: Any) = {
    val (xClass, expectedClass) =
      (x.asInstanceOf[AnyRef].getClass, classManifest[T].erasure)
    MatchResult(
      expectedClass.isAssignableFrom(xClass),
      xClass.getName + " is *not* instance of " + expectedClass.getName,
      xClass.getName + " is instance of "       + expectedClass.getName
    )
   }
}


object ConsistentChoiceSuite {

  val expectedType = new IsInstanceOfMatcher[ConsistentChoice[_]]

  val nThreads = 4
  val choicesPerThread = 10000

  val weightSafeChoiceMultiple = 200
  val weightErrTolerance = .05

  val nChoicesChecks = 100

  val nConsistencyChecks = 2500 * nThreads
  val rebalanceSafeChoiceMultiple = 250
  val rebalanceErrTolearance = .02

  def genTestSeq(len: Int) = {
    val rand = new RandGen with TemporallyRandom
    val (strMin, strMax) = (10, 100)
    Seq.fill(len)(rand.genString(strMin, strMax))
  }
}

class ConsistentChoiceSuite extends JUnitSuite with ShouldMatchers {

  import ConsistentChoiceSuite._


  @Test
  def testRetrieval() {
    val choices = ConsistentChoice("abc" -> 2, "xyz" -> 3)

    choices should have size 2 // assert(choices.size === 2)
    choices("abc") should equal (2.0)
    choices.get("xyz") should equal (Some(3.0))
  }

  @Test
  def testMap() {
    val choices = ConsistentChoice("abc" -> 2, "xyz" -> 3)
    val newChoices = choices.map { case (c, weight) => (c, weight * 2) }
    newChoices should have size 2
    newChoices("abc") should equal (4.0)
    newChoices.get("xyz") should equal (Some(6.0))

    newChoices should be (expectedType)
  }

  @Test
  def test_+-() {
    val choices = ConsistentChoice("abc" -> 2, "xyz" -> 3)
    val newChoices = choices + ("lmnop" -> 1.0) - "xyz"
    newChoices should equal (Map("abc" -> 2.0, "lmnop" -> 1.0))

    newChoices should be (expectedType)
  }

  @Test
  def test_++() {
    val choices = ConsistentChoice[String](100) ++
                    Seq("abc" -> 2.0, "lmnop" -> 1.0)
    choices should equal (Map("abc" -> 2.0, "lmnop" -> 1.0))

    choices should be (expectedType)
  }

  @Test
  def testWeights() {
    val (google, yahoo, bing) = (
        new URL("http://www.google.com/"),
        new URL("http://www.yahoo.com/"),
        new URL("http://www.bing.com/")
      )
    val choices = ConsistentChoice[URL](weightSafeChoiceMultiple) ++ Seq(
        google -> 4,
        yahoo  -> 2.0
      ) + (bing  -> 1)
    val total = (0.0 /: choices.values){ _ + _ }

    val counts = chooseConcurrently(choices, nThreads, choicesPerThread)
    val (pcts, totalCount) = calcDistribution(counts.toMap)

    print("* finished making %d choices".format(totalCount))
    println(" using weightings:\n" + choices.mkString("    ", "\n    ", "\n"))

    println("  expected results:\n" + choices.toSeq.map { case (c, weight) =>
      (c, weight / total)
    }.mkString("    ", "\n    ", "\n"))

    println("  actual results:\n" + pcts.mkString("    ", "\n    ", "\n"))
    pcts should have size choices.size
    for {
      (choice, weight) <- choices
    } pcts(choice) should be (weight / total plusOrMinus weightErrTolerance)
  }

/* 5/26/11: removed, since, althought test works fine under sbt, it hangs maven
 *
 *
  @Test
  def testChoiceSeq() {
    val (google, yahoo, bing) = (
        new URL("http://www.google.com/"),
        new URL("http://www.yahoo.com/"),
        new URL("http://www.bing.com/")
      )
    val choices = ConsistentChoice(
        google -> 4,
        yahoo  -> 2,
        bing   -> 1
      )

    val testSeq = genTestSeq(nChoicesChecks)
    for {
      testStr <- testSeq
    } {
      val choiceSeq = choices.choicesFor(testStr)
      val uniqueChoices = Set(choiceSeq : _* )
      uniqueChoices should have size (choices.size)
      choiceSeq should have length (choices.size)
    }
  }
*/

  @Test
  def testConsistency() {
    val choices = ConsistentChoice[Char](('a' to 'k').zipWithIndex.map { p =>
      (p._1, p._2.toDouble)
    } : _* )

    val testSeq = genTestSeq(nConsistencyChecks)
    val testChoiceResults = testSeq map { x => (x, choices.chooseFor(x)) }
    val inconsistentChoices = findInconsistentChoices(testChoiceResults,choices)
    inconsistentChoices should be (Seq.empty)

    println("* made %d choices consistently".format(testSeq.size))
  }

  @Test
  def testRebalanceConsistency() {
    val hosts = 0 to 9 map( _.toString ) map( "host" + _ )

    // NOTE: necessary prior to ConsistentChoice[T] override of `++`
    // val origChoices: ConsistentChoice[String] =
    val origChoices = ConsistentChoice[String](rebalanceSafeChoiceMultiple) ++
        (hosts map( (_, 1.0) ))

    val testSeq = genTestSeq(nConsistencyChecks)
    val origChoiceResults = testSeq map { x => (x, origChoices.chooseFor(x)) }

    val inconsistentRemovalChoices = {
      // simulate removal
      val newChoices = origChoices - "host2"
      findInconsistentChoices(origChoiceResults, newChoices)
    }

    val inconsistentAdditionChoices = {
      // simulate adding 'host'
      val newChoices = origChoices + ("host99" -> 1.0)
      findInconsistentChoices(origChoiceResults, newChoices)
    }

    println("* checking rebalance consistentency over %d choices".
        format(testSeq.size))

    val (expectRemConsistency, expectAddConsistency) =
          (1.0 / hosts.size, 1.0 / (hosts.size + 1))

    val (remConsistency, addConsistency) =
        (inconsistentRemovalChoices.size.toDouble  / nConsistencyChecks,
         inconsistentAdditionChoices.size.toDouble / nConsistencyChecks)

    println("  expected (removal, addition) consistency of approx. (%f, %f)".
        format(expectRemConsistency, expectAddConsistency))

    println("  actual (removal, addition) consistency: (%f, %f)".
        format(remConsistency, addConsistency))

    remConsistency should be (
        expectRemConsistency plusOrMinus rebalanceErrTolearance
      )

    addConsistency should be (
        expectAddConsistency plusOrMinus rebalanceErrTolearance
      )
  }


  protected def findInconsistentChoices[T](
      prevChoiceResults: Seq[(String, Option[T])],
      choices: ConsistentChoice[T]
    ): Seq[(String, Option[T], Option[T])] =
    for {
      (key, prevChoice) <- prevChoiceResults
      currChoice = choices.chooseFor(key)
      if prevChoice != currChoice
    } yield (key, prevChoice, currChoice)

  protected lazy val defaultKeyCalc = new (Int => String) {
    val (strMin, strMax) = (10, 100)
    val rand = new RandGen with TemporallyRandom

    def apply(i: Int) = rand.genString(strMin, strMax)
  }


  protected def chooseConcurrently[T](
      choices: ConsistentChoice[T],
      nThreads: Int,
      choicesPerThread: Int,
      calcKey: Int => String = defaultKeyCalc
    ): collection.Map[T, Long] = {

    val counts: ConcurrentMap[T, AtomicLong] =
        new ConcurrentHashMap[T, AtomicLong]

    def increment(x: T) {
      counts.get(x).orElse {
        counts.putIfAbsent(x, new AtomicLong).orElse {
          counts.get(x)
        }
      }.get.incrementAndGet()
    }

    val workPlace = Executors.newFixedThreadPool(nThreads)

    val (startTrigger, finalCompletion) =
        (new CountDownLatch(1), new CountDownLatch(nThreads))

    for (i <- 0 until nThreads)
      workPlace.execute(
        new Runnable {
          def run() {
            startTrigger.await()
            try {
              var i = 0;
              while (i < choicesPerThread) {
                val key = calcKey(i)
                val choice = choices.chooseFor(key)
                increment(choice.get)
                i += 1
              }
            } finally {
              finalCompletion.countDown()
            }
          }
        })

    startTrigger.countDown()
    finalCompletion.await()
    workPlace.shutdown()

    counts.map { case (c, count) => (c, count.get) }
  }

  protected def calcDistribution[T](counts: Map[T, Long]):
      (Map[T, Double], Long) = {
    val total = (0L /: counts.values){ _ + _ }
    val pcts = 
      if (total == 0) Map.empty[T, Double]
      else counts.toMap.map { case (x, count) => (x, count.toDouble / total) }
    (pcts, total)
  }
}
