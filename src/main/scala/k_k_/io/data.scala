/*
   file: k_k_/io/data.scala

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
package k_k_.io.data

import scala.io.Source


/**
 *  Ultimate base class of all data file exceptions.
 */
sealed abstract class Data_File_Exception(path: String, msg: String)
    extends RuntimeException(msg + ": '" + path + "'")

case class Not_Found_Exception(path: String)
    extends Data_File_Exception(path, "file not found")

sealed abstract class Invalid_Data_File(path: String, msg: String)
    extends Data_File_Exception(path, msg)

case class Unrecognized_File_Type(header_line: String, path: String)
    extends Invalid_Data_File(path, "unrecognized file header in: {" +
                                    header_line + "}")

case class Incompatible_Data_File_Version(file_ver: String,
                                          max_supported_ver: String,
                                          header_line: String,
                                          path: String)
    extends Invalid_Data_File(path, "incompatible version '" + file_ver +
                                    "' not readable by version '" +
                                    max_supported_ver + "' software in: {" +
                                    header_line + "}")

case class Unexpected_Data_File_Structure(lang_name: String, struct: String,
                                          header_line: String, path: String)
    extends Invalid_Data_File(path, "unexpected structure '" + struct +
                                    "' (lang: '" + lang_name + "') in: {" +
                                    header_line + "}")

case class Invalid_Data_File_Input(lang_name: String, struct: String,
                                   input_line: String, path: String)
    extends Invalid_Data_File(path, "unexpected input for '" + struct +
                                    "' (lang: '" + lang_name + "') data: {" +
                                    input_line + "}")


/**
 *  Abstract base class for reading lines from a formatted (data) file, skipping
 *  comments, etc.
 */
abstract class Lines_From_Data_File {

  final def get_content_lines: List[String] =
    open_data_file(path_name) match {
      case None => throw Not_Found_Exception(path_name)
      case Some(source) => try {
        val line_iter = source.getLines
        if (line_iter.isEmpty)
          throw Unrecognized_File_Type("", path_name)

        val content_only_filter = validate_header(line_iter.next,
                                                  valid_struct_tests_by_lang)
        // NOTE: toList on lazy iterator makes strict, since source will close!
        content_only_filter(line_iter).toList
      } finally {
        source.close()
      }
    }
        
  val path_name: String

  val valid_struct_tests_by_lang: Map[String, String => Boolean]

  protected val load_via: Class[_] = classOf[Lines_From_Data_File]

  protected def open_data_file(path: String): Option[Source] =
    Option(load_via.getResourceAsStream(path)).map(
      Source.fromInputStream(_) )


  final val software_version = List(1, 0)

  final def is_compatible_version(file_version: Seq[Int]): Boolean =
    (software_version zip file_version filter ( p => p._1 != p._2 )
                                          map ( p => p._1  > p._2 )).
      headOption.getOrElse(true)


  protected final val data_magic_bytes = "dA+@"

  /** validates compatability of data file version with running software, and
   *  of expected file structure, described in syntax specific to a cited
   *  language; throws exception on failure or returns a function on
   *  Iterator[String] to strip comments and whitespace and return only
   *  non-blank lines
   */
  protected final def validate_header(header_line: String,
                                      struct_pred_by_lang:
                                        Map[String, String => Boolean]):
      Iterator[String] => Iterator[String] = {
    val Parsed_Data_Header =
          """^(.) \Q%s\E,v.(\d+(?:\.\d+)+):\s*(\w+):\s*(.*?)\s*$""".format(
            data_magic_bytes ).r
    header_line match {
      case Parsed_Data_Header(comment_char, ver_str, lang, structure) =>
        val file_version = ver_str.split('.').map(_.toInt).toSeq
        if (!is_compatible_version(file_version))
          throw Incompatible_Data_File_Version(file_version.mkString("."),
                                               software_version.mkString("."),
                                               header_line,
                                               path_name)
        val (struct_id, struct_filter) = parse_struct_specific_info(structure)
        if (!struct_pred_by_lang.get(lang).map( _.apply(struct_id) == true ).
              getOrElse(false))
          throw Unexpected_Data_File_Structure(lang, structure,
                                               header_line, path_name)
        val Non_Comment_Non_Whitespace =
              """^\s*((?:[^\\\Q%s\E]+|(?:\\.?))+).*""".format(comment_char).r
        iter: Iterator[String] => iter.collect {
          case Non_Comment_Non_Whitespace(data) =>
            // NOTE: need to remove all 'escape' backslashes
            data.trim().replaceAll("""\\(.)""", "$1")
        } map { struct_filter(_) }
      case _ => throw Unrecognized_File_Type(header_line, path_name)
    }
  }

  protected def parse_struct_specific_info(structure_tag: String):
      (String, String => String) =
    (structure_tag, identity)
}


/**
 *  Reads lines from a formatted (data) file, returning a `Seq[String]`,
 *  skipping comments, etc.
 */
class String_Seq_From_Data_File(val path_name: String)
    extends Lines_From_Data_File {

  val valid_struct_tests_by_lang = Map(("scala",
                                        (s: String) => s == "Seq[String]"))

  def get_seq: Seq[String] =
    get_content_lines
}


/**
 *  Defines defaults for companion class.
 */
object Assoc_Seq_From_Data_File {

  val default_field_sep = ","
}

/**
 *  Abstract base class for reading (key, value) pairs from a formatted (data)
 *  file, returning a `Seq[(Key_T, Val_T)]` or a `Map[Key_T, Val_T]` (in which
 *  case the final value for a given key is the one returned).  Skips comments,
 *  etc.
 */
abstract class Assoc_Seq_From_Data_File[Key_T, Val_T](val path_name: String)
    extends Lines_From_Data_File {

  final def get_seq: Seq[(Key_T, Val_T)] =
    get_content_line_pairs map { convert(_) }

  final def get_mappings: Map[Key_T, Val_T] =
    Map(get_seq : _* )

  final def get_content_line_pairs: List[(String, String)] =
    get_content_lines.map { parse_content_line_pair(_) }

  val valid_struct_tests_by_lang: Map[String, String => Boolean] =
        Map(("scala",
             (s: String) => s.replaceAll("\\s+", "") ==
                              "Map[" + pair_encoding_no_spaces + "]" ))


  protected val pair_encoding_no_spaces: String

  protected def convert(pair: (String, String)): (Key_T, Val_T)


  override
  protected def parse_struct_specific_info(structure_tag: String):
      (String, String => String) = {
    val Struct_Info = """^\s*(?x)  (.*?)  (?: \(  ([^\)]+)  \) )  \s*$""".r
    structure_tag match {
      case Struct_Info(struct_name, field_sep) =>
        //!!!!TODO: examine key for escaped field sep (however that'd look)!!!
        (struct_name,
         s => "[" + field_sep + "]" + s)
      case _ =>
        (structure_tag,
         s => "[" + Assoc_Seq_From_Data_File.default_field_sep + "]"+ s)
    }
  }

  protected def parse_content_line_pair(line: String): (String, String) = {
    val Content_Pair = """^(?x) \[  ([^\]]+)  \]  (.+?)  \1  (.*)  $""".r
    line match {
      case Content_Pair(_, key, value) => (key, value)
      case _ => throw Invalid_Data_File_Input("scala",
                                        "Map[" + pair_encoding_no_spaces + "]",
                                              line, path_name)
    }
  }
}

/**
 *  Reads (key, value) pairs of `(Char, Double)` from a formatted (data) file,
 *  returning a `Seq[(Char, Double)]` or a `Map[Char, Double]` (in which case
 *  the final value for key is the one returned).  Skips comments, etc.
 */
class Char_Double_Seq_From_Data_File(fpath: String)
    extends Assoc_Seq_From_Data_File[Char, Double](fpath) {

  protected val pair_encoding_no_spaces = "Char,Double"

  protected def convert(pair: (String, String)): (Char, Double) =
    (pair._1.head, pair._2.toDouble)
}


/* example data file for use with `String_Seq_From_Data_File`:

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


/* example data file for use with `Char_Double_Seq_From_Data_File`:

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
