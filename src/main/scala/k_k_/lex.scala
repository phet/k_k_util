/*
   file: k_k_/lex.scala

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
package k_k_.lex {


/**
 *  Functions for parsing (Scala) identifiers.
 */
object Ident {

  /**
   *  Returns unqualified name (w/o package elements) of the class (or object).
   * 
   *  The name returned for an object is identical to what would be returned for
   *  its companion class (i.e. the trailing '$' is dropped).
   */
  def calc_unqual_name(clazz: Class[_]): String = {
    val Parse_Class_Name = """.*?([^\.]+?)\$?$""".r
    clazz.getName match {
      case Parse_Class_Name(unqual_name) => unqual_name
      case fq_name => fq_name // oh well, return name as-is
    }
  }

  /**
   *  Returns unqualified name (w/o package elements) of the instance's class.
   *
   *  The name returned for an object is identical to what would be returned for
   *  its companion class (i.e. the trailing '$' is dropped).
   */
  def calc_unqual_class_name(obj: Any): String =
    obj match {
      case clazz : Class[_] => calc_unqual_name(clazz)
      case x => calc_unqual_name(x.asInstanceOf[AnyRef].getClass)
    }
}

}
