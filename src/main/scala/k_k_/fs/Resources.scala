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
   *  @return every classpath resource name beginning with the file path prefix
   *  given, as reported by the classloader of `loadClass` (defaults to this
   *  class).
   * 
   *  NOTE: `resourcePrefix` shall be interpreted as using separator char `'/'`
   */
  def matching(
      resourcePrefix: String,
      loadClass: Class[_] = classOf[Resources]
    ): Seq[String] = {
    val SEP = '/' 

    def recurseDir(resourcePath: String, file: File): Seq[String] =
      if (file.isDirectory)
        resourcePath +: file.list.toSeq.flatMap { name =>
          recurseDir(resourcePath + SEP + name, new File(file, name))
        }
      else
        Seq(resourcePath)

    val (parentDir, filePrefix0) = resourcePrefix.lastIndexOf(SEP) match {
      case -1 => (resourcePrefix, "")
      case i  => resourcePrefix.splitAt(i)
    }
    val filePrefix = filePrefix0.dropWhile( _ == SEP )
    Option(loadClass.getResource(parentDir)) match {
      case Some(url) if url.getProtocol == "file" =>
        val file = new File(url.toURI)
        if (file.isDirectory)
          file.list.toSeq.filter( _.startsWith(filePrefix) ).flatMap { name =>
            recurseDir(parentDir + SEP + name, new File(file, name))
          }
        else if (filePrefix == "")
          Seq(parentDir)
        else
          Seq.empty
      case Some(url) if url.getProtocol == "jar" =>
        val FileURLPath = """^(?i)file:(?:/([A-Z]:/[^!]+)|(/[^!]+))!.*$""".r
        val jarFpath = url.getPath match {
          case FileURLPath(win, _)  if win ne null  => win
          case FileURLPath(_, unix) if unix ne null => unix
          case _ => ""
        }
        val (startPrefix, correction) =
          if (resourcePrefix.headOption == Some('/'))
            (resourcePrefix.tail, (s: String) => '/' +: s)
          else
            (resourcePrefix, identity[String] _)

        val jar = new JarFile(URLDecoder.decode(jarFpath, "UTF-8"))
        try {
          jar.entries.map( _.getName ).filter {
            _.startsWith(startPrefix)
          }.map(correction).toList // force strict iteration
        } finally {
          jar.close()
        }
      case _ => Seq.empty
    }
  }
}

/**
 *  Empty, unusable class exists solely for companion object to take `classOf[]`
 */
class Resources private()
