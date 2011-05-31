/*
   file: k_k_/io/stream.scala

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
package k_k_.io.stream

import java.io.{InputStream, OutputStream,
                File, FileInputStream, FileOutputStream,
                Reader, InputStreamReader,
                ByteArrayOutputStream}
import java.nio.charset.{Charset, CharsetDecoder}


/**
 *  Functions for bulk handling byte-oriented input.
 */
object Byte_Input {

  /**
   *  Read entire file into `Array[Byte]`
   */
  def slurp_file(file: File): Array[Byte] = {
    val fos = new FileInputStream(file)
    try {
      slurp_stream(fos)
    } finally { fos.close }
  }
  
  /**
   *  Read entire contents of `InputStream` into `Array[Byte]`; `InputStream`
   *  not closed
   */
  def slurp_stream(is: InputStream): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    try {
      copy_stream(is, bos)
      bos.toByteArray
    } finally { bos.close }
  }

  /**
   *  Write entire contents of `InputStream` to `File`; `InputStream` not closed
   */
  def copy_stream(is: InputStream, file: File): Boolean = {
    val os = new FileOutputStream(file)
    try {
      copy_stream(is, os)
    } finally { os.close }
  }

  /**
   *  Write all contents of `InputStream` to `OutputStream`; neither stream
   *  closed
   */
  def copy_stream(is: InputStream, os: OutputStream, chunk_size: Int = 4096):
      Boolean = {
    val buff = new Array[Byte](chunk_size)

    def siphon_bytes {
      val n = is.read(buff)
      if (n > 0)
        os.write(buff, 0, n)
      if (n >= 0)
        siphon_bytes // keep siphoning
    }

    siphon_bytes
    true
  }
}


/**
 *  Functions for bulk handling character-oriented input.
 */
object Char_Input {

  /**
   *  Read entire text `File` into `String`.
   */
  def slurp_text_file(file: File, charset_name: String = "UTF-8"): String = {
    val fos = new FileInputStream(file)
    try {
      slurp_text_stream(fos, charset_name)
    } finally { fos.close }
  }

  /**
   *  Read entire text `File` into `String`.
   */
  def slurp_text_file(file: File, charset: Charset): String = {
    val fos = new FileInputStream(file)
    try {
      slurp_text_stream(fos, charset)
    } finally { fos.close }
  }

  /**
   *  Read entire text `File` into `String`.
   */
  def slurp_text_file(file: File, charset_decoder: CharsetDecoder): String = {
    val fos = new FileInputStream(file)
    try {
      slurp_text_stream(fos, charset_decoder)
    } finally { fos.close }
  }


  /**
   *  Read entire contents of `InputStream` into `String`; `InputStream` not
   *  closed
   */
  def slurp_text_stream(is: InputStream, charset_name: String = "UTF-8"):
      String =
    slurp_reader(new InputStreamReader(is, charset_name))

  /**
   *  Read entire contents of `InputStream` into `String`; `InputStream` not
   *  closed
   */
  def slurp_text_stream(is: InputStream, charset: Charset):
      String =
    slurp_reader(new InputStreamReader(is, charset))

  /**
   *  Read entire contents of `InputStream` into `String`; `InputStream` not
   *  closed
   */
  def slurp_text_stream(is: InputStream, charset_decoder: CharsetDecoder):
      String =
    slurp_reader(new InputStreamReader(is, charset_decoder))


  /**
   *  Read entire contents of `Reader` into `String`; `Reader` not closed
   */
  def slurp_reader(in: Reader): String = {
    val builder = new StringBuilder
    copy_reader(in, builder)
    builder.toString
  }

  /**
   *  Write all contents of `Reader` to `StringBuilder`; `Reader` not closed
   */
  def copy_reader(in: Reader, builder: StringBuilder, chunk_size: Int = 4096):
      Boolean = {
    val buff = new Array[Char](chunk_size)

    def siphon_bytes {
      val n = in.read(buff)
      if (n > 0)
        builder.appendAll(buff, 0, n)
      if (n >= 0)
        siphon_bytes // keep siphoning
    }

    siphon_bytes
    true
  }
}
