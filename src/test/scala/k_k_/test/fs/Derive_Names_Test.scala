/*
   file: k_k_/test/fs/Derive_Names_Test.scala

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
package k_k_.test.fs

import org.junit._
import Assert._
import org.scalatest.junit.JUnitSuite

import java.io.File

import k_k_.fs.{Derive_Name, Ensure}


class Derive_Names_Test extends JUnitSuite {

  @Test
  def test_derive_name_via_suffix {
    // set basis: ensure that def'n of files has not changed
    assertEquals(new File("target/test-classes/images/horizontal01.jpg"),
                 files(0))

    val suffix = "_foo"

    assertEquals(new File("target/test-classes/images/horizontal01_foo.jpg"),
                 Derive_Name.via_suffix(files(0), suffix))
    assertEquals(new File("target/test-classes/images_foo"),
                 Derive_Name.via_suffix(files(0).getParentFile, suffix))
    assertEquals(new File("target/test-classes/imgs/h_foo"),
                 Derive_Name.via_suffix(new File("target/test-classes/imgs/h"),
                                        suffix))
  }

  @Test
  def test_derive_name_via_subdir {
    // set basis: ensure that def'n of files has not changed
    assertEquals(new File("target/test-classes/images/horizontal01.jpg"),
                 files(0))

    val subdir = "BArDir"

    assertEquals(new File("target/test-classes/images/BArDir/horizontal01.jpg"),
                 Derive_Name.via_subdir(files(0), subdir))

    assertTrue(Ensure.parent_dir_exists(files(0)))
    assertEquals(new File("target/test-classes/images/BArDir"),
                 Derive_Name.via_subdir(files(0).getParentFile, subdir))
    assertEquals(new File("target/test-classes/images/BArDir"),
                 Derive_Name.via_subdir(new File(files(0).getParent +
                                                 File.separator), subdir))

    assertEquals(new File("target/test-classes/imgs/BArDir/h"),
                 Derive_Name.via_subdir(new File("target/test-classes/imgs/h"),
                                        subdir))

    assertEquals(new File("target/test-classes/images/BArDir"),
                 Derive_Name.via_subdir(new File(files(0).getParent +
                                                 File.separator),
				        subdir + File.separator))
    assertEquals(new File("target/test-classes/imgs/BArDir/h"),
                 Derive_Name.via_subdir(new File("target/test-classes/imgs/h"),
                                        subdir + File.separator))
  }


  protected val files: Seq[File] =
    Seq(new File("target/test-classes/images/horizontal01.jpg"),
        new File("target/test-classes/images/vertical01.jpg"))
}
