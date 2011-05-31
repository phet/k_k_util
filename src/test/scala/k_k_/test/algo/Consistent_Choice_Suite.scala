/*
   file: k_k_/test/algo/Consistent_Choice_Suite.scala

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

import k_k_.algo.Consistent_Choice


// NOTE: since type class bound ClassManifest (not Manifest), unable to verify
// type ctor params (i.e. HOType[_] verifiable, but not ArgT in HOType[ArgT])
class IsInstanceOfMatcher[T: ClassManifest] extends BeMatcher[Any] {
  def apply(x: Any) = {
    val (x_class, expected_class) = (x.asInstanceOf[AnyRef].getClass,
                                     classManifest[T].erasure)
    MatchResult(
      expected_class.isAssignableFrom(x_class),
      x_class.getName + " is *not* instance of " + expected_class.getName,
      x_class.getName + " is instance of "       + expected_class.getName
    )
   }
}


object Consistent_Choice_Suite {

  val expected_type = new IsInstanceOfMatcher[Consistent_Choice[_]]

  val n_threads = 4
  val choices_per_thread = 10000

  val weight_safe_choice_multiple = 200
  val weight_err_tolerance = .05

  val n_choices_checks = 100

  val n_consistency_checks = 2500 * n_threads
  val rebalance_safe_choice_multiple = 250
  val rebalance_err_tolearance = .02

  def gen_test_seq(len: Int) = {
    val rand = new Rand_Gen with Temporally_Random
    val (str_min, str_max) = (10, 100)
    Seq.fill(len)(rand.gen_string(str_min, str_max))
  }
}

class Consistent_Choice_Suite extends JUnitSuite with ShouldMatchers {

  import Consistent_Choice_Suite._


  @Test
  def test_retrieval {
    val choices = Consistent_Choice("abc" -> 2, "xyz" -> 3)

    choices should have size 2 // assert(choices.size === 2)
    choices("abc") should equal (2.0)
    choices.get("xyz") should equal (Some(3.0))
  }

  @Test
  def test_map {
    val choices = Consistent_Choice("abc" -> 2, "xyz" -> 3)
    val new_choices = choices.map { case (c, weight) => (c, weight * 2) }
    new_choices should have size 2
    new_choices("abc") should equal (4.0)
    new_choices.get("xyz") should equal (Some(6.0))

    new_choices should be (expected_type)
  }

  @Test
  def test_+- {
    val choices = Consistent_Choice("abc" -> 2, "xyz" -> 3)
    val new_choices = choices + ("lmnop" -> 1.0) - "xyz"
    new_choices should equal (Map("abc" -> 2.0, "lmnop" -> 1.0))

    new_choices should be (expected_type)
  }

  @Test
  def test_++ {
    val choices = Consistent_Choice[String](100) ++
                    Seq("abc" -> 2.0, "lmnop" -> 1.0)
    choices should equal (Map("abc" -> 2.0, "lmnop" -> 1.0))

    choices should be (expected_type)
  }

  @Test
  def test_weights {
    val (google, yahoo, bing) = (new URL("http://www.google.com/"),
                                 new URL("http://www.yahoo.com/"),
                                 new URL("http://www.bing.com/"))
    val choices = Consistent_Choice[URL](weight_safe_choice_multiple) ++
                    Seq(google -> 4,
                        yahoo  -> 2.0) +
                    (bing  -> 1)
    val total = (0.0 /: choices.values){ _ + _ }

    val counts = choose_concurrently(choices, n_threads, choices_per_thread)
    val (pcts, total_count) = calc_distribution(counts.toMap)

    print("* finished making %d choices".format(total_count))
    println(" using weightings:\n" + choices.mkString("    ", "\n    ", "\n"))

    println("  expected results:\n" +
            choices.toSeq.map { case (c, weight) => (c, weight / total) }.
              mkString("    ", "\n    ", "\n"))

    println("  actual results:\n" + pcts.mkString("    ", "\n    ", "\n"))
    pcts should have size choices.size
    for {
      (choice, weight) <- choices
    } pcts(choice) should be (weight / total plusOrMinus weight_err_tolerance)
  }

/* 5/26/11: removed, since, althought test works fine under sbt, it hangs maven
 *
 *
  @Test
  def test_choice_seq {
    val (google, yahoo, bing) = (new URL("http://www.google.com/"),
                                 new URL("http://www.yahoo.com/"),
                                 new URL("http://www.bing.com/"))
    val choices = Consistent_Choice(google -> 4,
                                    yahoo  -> 2,
                                    bing   -> 1)

    val test_seq = gen_test_seq(n_choices_checks)
    for {
      test_str <- test_seq
    } {
      val choice_seq = choices.choices_for(test_str)
      val unique_choices = Set(choice_seq : _* )
      unique_choices should have size (choices.size)
      choice_seq should have length (choices.size)
    }
  }
*/

  @Test
  def test_consistency {
    val choices = Consistent_Choice[Char](('a' to 'k').zipWithIndex.map { p =>
          (p._1, p._2.toDouble)
      } : _* )

    val test_seq = gen_test_seq(n_consistency_checks)
    val test_choice_results = test_seq map { x => (x, choices.choose_for(x)) }
    val inconsistent_choices =
          find_inconsistent_choices(test_choice_results, choices)
    inconsistent_choices should be (Seq.empty)

    println("* made %d choices consistently".format(test_seq.size))
  }

  @Test
  def test_rebalance_consistency {
    val hosts = 0 to 9 map( _.toString ) map( "host" + _ )

    // NOTE: necessary prior to Consistent_Choice[T] override of `++`
    // val orig_choices: Consistent_Choice[String] =
    val orig_choices =
          Consistent_Choice[String](rebalance_safe_choice_multiple) ++
            (hosts map( (_, 1.0) ))

    val test_seq = gen_test_seq(n_consistency_checks)
    val orig_choice_results =
          test_seq map { x => (x, orig_choices.choose_for(x)) }

    val inconsistent_removal_choices = {
      // simulate removal
      val new_choices = orig_choices - "host2"
      find_inconsistent_choices(orig_choice_results, new_choices)
    }

    val inconsistent_addition_choices = {
      // simulate adding 'host'
      val new_choices = orig_choices + ("host99" -> 1.0)
      find_inconsistent_choices(orig_choice_results, new_choices)
    }

    println("* checking rebalance consistentency over %d choices".
                format(test_seq.size))

    val (expect_rem_consistency, expect_add_consistency) =
          (1.0 / hosts.size, 1.0 / (hosts.size + 1))

    val (rem_consistency, add_consistency) =
      (inconsistent_removal_choices.size.toDouble  / n_consistency_checks,
       inconsistent_addition_choices.size.toDouble / n_consistency_checks)

    println("  expected (removal, addition) consistency of approx. (%f, %f)".
                format(expect_rem_consistency, expect_add_consistency))

    println("  actual (removal, addition) consistency: (%f, %f)".
                format(rem_consistency, add_consistency))

    rem_consistency should be (
      expect_rem_consistency plusOrMinus rebalance_err_tolearance)

    add_consistency should be (
      expect_add_consistency plusOrMinus rebalance_err_tolearance)
  }


  protected def find_inconsistent_choices[T](prev_choice_results:
                                               Seq[(String, Option[T])],
                                             choices: Consistent_Choice[T]):
      Seq[(String, Option[T], Option[T])] =
    for {
      (key, prev_choice) <- prev_choice_results
      curr_choice = choices.choose_for(key)
      if prev_choice != curr_choice
    } yield (key, prev_choice, curr_choice)

  protected lazy val default_key_calc = new (Int => String) {
    val (str_min, str_max) = (10, 100)
    val rand = new Rand_Gen with Temporally_Random

    def apply(i: Int) =
      rand.gen_string(str_min, str_max)
  }


  protected def choose_concurrently[T](choices: Consistent_Choice[T],
                                       n_threads: Int, choices_per_thread: Int,
                                       calc_key: Int => String =
                                         default_key_calc):
      collection.Map[T, Long] = {

    val counts: ConcurrentMap[T, AtomicLong] =
          new ConcurrentHashMap[T, AtomicLong]

    def increment(x: T) {
      counts.get(x).orElse(counts.putIfAbsent(x, new AtomicLong).orElse(
                           counts.get(x))).get.incrementAndGet
    }

    val work_place = Executors.newFixedThreadPool(n_threads)

    val (start_trigger, final_completion) = (new CountDownLatch(1),
                                             new CountDownLatch(n_threads))

    for (i <- 0 until n_threads)
      work_place.execute(
        new Runnable {
          def run {
            start_trigger.await
            try {
              var i = 0;
              while (i < choices_per_thread) {
                val key = calc_key(i)
                val choice = choices.choose_for(key)
                increment(choice.get)
                i += 1
              }
            } finally {
              final_completion.countDown
            }
          }
        })

    start_trigger.countDown
    final_completion.await
    work_place.shutdown

    counts.map { case (c, count) => (c, count.get) }
  }

  protected def calc_distribution[T](counts: Map[T, Long]):
      (Map[T, Double], Long) = {
    val total = (0L /: counts.values){ _ + _ }
    val pcts = 
      if (total == 0) Map.empty[T, Double]
      else counts.toMap.map { case (x, count) => (x, count.toDouble / total) }
    (pcts, total)
  }
}
