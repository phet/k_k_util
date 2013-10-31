/*
   file: k_k_/io/data.scala

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
package k_k_.io.data

import scala.io.Source


/**
 *  Ultimate base class of all data file exceptions.
 */
sealed abstract class DataFileException(path: String, msg: String)
    extends RuntimeException(msg + ": '" + path + "'")

case class NotFoundException(path: String)
    extends DataFileException(path, "file not found")

sealed abstract class InvalidDataFile(path: String, msg: String)
    extends DataFileException(path, msg)

case class UnrecognizedFileType(headerLine: String, path: String)
    extends InvalidDataFile(
        path,
        "unrecognized file header in: {%s}".format(headerLine)
      )

case class IncompatibleDataFileVersion(
    fileVer: String,
    maxSupportedVer: String,
    headerLine: String,
    path: String
  ) extends InvalidDataFile(
    path,
    "incompatible version '%s' not readable by version '%s' software in: {%s}".
        format(fileVer, maxSupportedVer, headerLine)
  )

case class UnexpectedDataFileStructure(
    langName: String,
    struct: String,
    headerLine: String,
    path: String
  ) extends InvalidDataFile(
    path,
    "unexpected structure '%s' (lang: '%s') in: {%s}".format(
        struct,
        langName,
        headerLine
    ))

case class InvalidDataFileInput(
    langName: String,
    struct: String,
    inputLine: String,
    path: String
  ) extends InvalidDataFile(
    path,
    "unexpected input for '%s' (lang: '%s') data: {%s}".format(
        struct,
        langName,
        inputLine
    ))


/**
 *  Abstract base class for reading lines from a formatted (data) file, skipping
 *  comments, etc.
 */
abstract class LinesFromDataFile {

  final def getContentLines: List[String] = openDataFile(pathName) match {
    case None => throw NotFoundException(pathName)
    case Some(source) => try {
      val lineIter = source.getLines
      if (lineIter.isEmpty) throw UnrecognizedFileType("", pathName)

      val contentOnlyFilter =
          validateHeader(lineIter.next, validStructTestsByLang)
      // NOTE: toList on lazy iterator makes strict, since source will close!
      contentOnlyFilter(lineIter).toList
    } finally {
      source.close()
    }
  }
        
  val pathName: String

  val validStructTestsByLang: Map[String, String => Boolean]

  protected val loadVia: Class[_] = classOf[LinesFromDataFile]

  protected def openDataFile(path: String): Option[Source] =
    Option(loadVia.getResourceAsStream(path)).map( Source.fromInputStream _ )


  final val softwareVersion = List(1, 0)

  final def isCompatibleVersion(fileVersion: Seq[Int]): Boolean =
    softwareVersion.zip(fileVersion).filter { p =>
      p._1 != p._2
    }.map { p =>
      p._1  > p._2
    }.headOption.getOrElse(true)


  protected final val dataMagicBytes = "dA+@"

  /** validates compatability of data file version with running software, and
   *  of expected file structure, described in syntax specific to a cited
   *  language; throws exception on failure or returns a function on
   *  Iterator[String] to strip comments and whitespace and return only
   *  non-blank lines
   */
  protected final def validateHeader(
      headerLine: String,
      structPredByLang: Map[String, String => Boolean]
    ): Iterator[String] => Iterator[String] = {
    val ParsedDataHeader =
        """^(.) \Q%s\E,v.(\d+(?:\.\d+)+):\s*(\w+):\s*(.*?)\s*$""".format(
            dataMagicBytes
          ).r
    headerLine match {
      case ParsedDataHeader(commentChar, verStr, lang, struct) =>
        val fileVersion = verStr.split('.').map(_.toInt).toSeq
        if (!isCompatibleVersion(fileVersion))
          throw IncompatibleDataFileVersion(
              fileVersion.mkString("."),
              softwareVersion.mkString("."),
              headerLine,
              pathName
            )
        val (structId, structFilter) = parseStructSpecificInfo(struct)
        if (!structPredByLang.get(lang).map {
              _.apply(structId) == true
            }.getOrElse(false))
          throw UnexpectedDataFileStructure(lang, struct, headerLine, pathName)
        val NonCommentNonWhitespace =
              """^\s*((?:[^\\\Q%s\E]+|(?:\\.?))+).*""".format(commentChar).r
        (iter: Iterator[String]) => iter.collect {
          case NonCommentNonWhitespace(data) =>
            // NOTE: need to remove all 'escape' backslashes
            data.trim().replaceAll("""\\(.)""", "$1")
        } map { structFilter(_) }
      case _ => throw UnrecognizedFileType(headerLine, pathName)
    }
  }

  protected def parseStructSpecificInfo(structureTag: String):
      (String, String => String) =
    (structureTag, identity)
}


/**
 *  Reads lines from a formatted (data) file, returning a `Seq[String]`,
 *  skipping comments, etc.
 */
class StringSeqFromDataFile(val pathName: String)
    extends LinesFromDataFile {

  val validStructTestsByLang = Map(
      "scala" -> ((s: String) => s == "Seq[String]")
    )

  def getSeq: Seq[String] = getContentLines
}


/**
 *  Defines defaults for companion class.
 */
object AssocSeqFromDataFile {

  val defaultFieldSep = ","
}

/**
 *  Abstract base class for reading (key, value) pairs from a formatted (data)
 *  file, returning a `Seq[(KeyT, ValT)]` or a `Map[KeyT, ValT]` (in which
 *  case the final value for a given key is the one returned).  Skips comments,
 *  etc.
 */
abstract class AssocSeqFromDataFile[KeyT, ValT](val pathName: String)
    extends LinesFromDataFile {

  final def getSeq: Seq[(KeyT, ValT)] = getContentLinePairs map { convert(_) }

  final def getMappings: Map[KeyT, ValT] = Map(getSeq : _* )

  final def getContentLinePairs: List[(String, String)] =
    getContentLines.map { parseContentLinePair(_) }

  val validStructTestsByLang: Map[String, String => Boolean] = Map(
      "scala" -> ((s: String) =>
        s.replaceAll("\\s+", "") == "Map[" + pairEncodingNoSpaces + "]")
    )


  protected val pairEncodingNoSpaces: String

  protected def convert(pair: (String, String)): (KeyT, ValT)


  override protected def parseStructSpecificInfo(structureTag: String):
      (String, String => String) = {
    val StructInfo = """^\s*(?x)  (.*?)  (?: \(  ([^\)]+)  \) )  \s*$""".r
    structureTag match {
      case StructInfo(structName, fieldSep) =>
        //!!!!TODO: examine key for escaped field sep (however that'd look)!!!
        (structName, s => "[" + fieldSep + "]" + s)
      case _ =>
        (structureTag, s => "[" + AssocSeqFromDataFile.defaultFieldSep + "]"+ s)
    }
  }

  protected def parseContentLinePair(line: String): (String, String) = {
    val ContentPair = """^(?x) \[  ([^\]]+)  \]  (.+?)  \1  (.*)  $""".r
    line match {
      case ContentPair(_, key, value) => (key, value)
      case _ => throw InvalidDataFileInput(
          "scala",
          "Map[%s]".format(pairEncodingNoSpaces),
          line,
          pathName
        )
    }
  }
}

/**
 *  Reads (key, value) pairs of `(Char, Double)` from a formatted (data) file,
 *  returning a `Seq[(Char, Double)]` or a `Map[Char, Double]` (in which case
 *  the final value for key is the one returned).  Skips comments, etc.
 */
class CharDoubleSeqFromDataFile(fpath: String)
    extends AssocSeqFromDataFile[Char, Double](fpath) {

  protected val pairEncodingNoSpaces = "Char,Double"

  protected def convert(pair: (String, String)): (Char, Double) =
    (pair._1.head, pair._2.toDouble)
}


/* example data file for use with `StringSeqFromDataFile`:

# dA+@,v.1.0:scala: Seq[String]

# W3C CSS2 named colors, the so-called 'X11 Colors'; compiled from:
#   "Learn SVG: The Web Graphics Standard"; "Appendix A: sRGB Colors";
#     by Jon Frost, Stefan Goessner and Michel Hirtzler,
#     revisited by Robert DiBlasi and Tobias Reif
#   http://www.learnsvg.com/
#   english translation: http://www.learnsvg.com/dnld/eBookFullAll.zip

AliceBlue
AntiqueWhite

# [...]

*/


/* example data file for use with `CharDoubleSeqFromDataFile`:

# dA+@,v.1.0:scala: Map[Char, Double](:)

# per-character font measurements for:
#   font-family: 'Times New Roman'
#   font-weight: 400 (normal)
#   (font-size: 10px)
#
# data subset: ascii printable non-whitespace chars + ascii single-space
#
# measurements collected under: apache-batik v1.7

A:7.2216796875
B:6.669921875
C:6.669921875
D:7.2216796875

# [...]

*/
