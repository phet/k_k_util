/*
   file: k_k_/lex.scala

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
package k_k_.lex {


/**
 *  Functions for parsing (Scala) identifiers.
 */
object Ident {

  /**
   *  @return unqualified name (w/o package elements) of the class (or object).
   * 
   *  The name returned for an object is identical to what would be returned for
   *  its companion class (i.e. the trailing '$' is dropped).
   */
  def calcUnqualName(clazz: Class[_]): String = {
    val ParseClassName = """.*?([^\.]+?)\$?$""".r
    clazz.getName match {
      case ParseClassName(unqualName) => unqualName
      case fqName => fqName // oh well, return name as-is
    }
  }

  /**
   *  @return unqualified name (w/o package elements) of the instance's class.
   *
   *  The name returned for an object is identical to what would be returned for
   *  its companion class (i.e. the trailing '$' is dropped).
   */
  def calcUnqualClassName(obj: Any): String =
    obj match {
      case clazz: Class[_] => calcUnqualName(clazz)
      case x => calcUnqualName(x.asInstanceOf[AnyRef].getClass)
    }
}

}
