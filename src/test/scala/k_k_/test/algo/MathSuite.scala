/*
   file: k_k_/test/algo/MathSuite.scala

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

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import k_k_.algo.Math


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MathSuite extends FunSuite with ShouldMatchers {

  class Funky(val n: Int, name: String) {
    override val toString = "Funky(%d, \"%s\")".format(n, name)
  }

  implicit val FunkyOrdering = Ordering.Int.on[Funky]( _.n )

  test("maxMin") {
    Math.maxMin(16, 9)      should be ((16, 9))
    Math.maxMin(10, 29)     should be ((29, 10))
    Math.maxMin(15.2, 15.9) should be ((15.9, 15.2))

    val (a, b) = (new Funky(16, "a"), new Funky(16, "b"))
    Math.maxMin(a, b) should be ((a, b))
  }


  test("minMax") {
    Math.minMax(16, 9)      should be ((9, 16))
    Math.minMax(10, 29)     should be ((10, 29))
    Math.minMax(15.2, 15.9) should be ((15.2, 15.9))

    val (a, b) = (new Funky(16, "a"), new Funky(16, "b"))
    Math.minMax(a, b) should be ((a, b))
  }
}
