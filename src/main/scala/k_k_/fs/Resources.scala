/*
   file: k_k_/fs/Resources.scala

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
package k_k_.fs

import java.io.File
import java.net.URLDecoder
import java.util.jar.JarFile

import scala.collection.JavaConversions._


/**
 *  Utility functions for processing classpath resources.
 */
object Resources {

  /**
   *  Returns every classpath resource name beginning with the file path prefix
   *  given, as reported by the classloader of `load_class` (defaults to this
   *  class).
   * 
   *  NOTE: `resource_prefix` shall be interpreted as using separator char `'/'`
   */
  def matching(resource_prefix: String,
               load_class: Class[_] = classOf[Resources]): Seq[String] = {
    val SEP = '/' 

    def recurse_dir(resource_path: String, file: File): Seq[String] =
      if (file.isDirectory)
        resource_path +:
          file.list.toSeq.flatMap( name =>
                                     recurse_dir(resource_path + SEP + name,
                                                 new File(file, name)) )
      else
        Seq(resource_path)

    val (parent_dir, file_prefix0) = resource_prefix.lastIndexOf(SEP) match {
      case -1 => (resource_prefix, "")
      case i  => resource_prefix.splitAt(i)
    }
    val file_prefix = file_prefix0.dropWhile( _ == SEP )
    Option(load_class.getResource(parent_dir)) match {
      case Some(url) if url.getProtocol == "file" =>
        val file = new File(url.toURI)
        if (file.isDirectory)
          file.list.toSeq.filter( _.startsWith(file_prefix) ).
                         flatMap( name =>
                                    recurse_dir(parent_dir + SEP + name,
                                                new File(file, name)) )
        else if (file_prefix == "")
          Seq(parent_dir)
        else
          Seq.empty
      case Some(url) if url.getProtocol == "jar" =>
        val File_URL_Path = """^(?i)file:(?:/([A-Z]:/[^!]+)|(/[^!]+))!.*$""".r
        val jar_fpath = url.getPath match {
          case File_URL_Path(win, _)  if win ne null  => win
          case File_URL_Path(_, unix) if unix ne null => unix
          case _ => ""
        }
        val (start_prefix, correction) =
              if (resource_prefix.headOption == Some('/'))
                (resource_prefix.tail, (s: String) => '/' +: s)
              else
                (resource_prefix, identity[String] _)
        val jar = new JarFile(URLDecoder.decode(jar_fpath, "UTF-8"))
        try {
          jar.entries.map( _.getName ).
                   filter( _.startsWith(start_prefix) ).
                      map( correction(_) ).toList // force strict iteration
        } finally {
          jar.close
        }
      case _ => Seq.empty
    }
  }
}

/**
 *  Empty, unusable class exists solely for companion object to take `classOf[]`
 */
class Resources private()
