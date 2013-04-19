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

import java.io._
import java.nio.charset.{Charset, CharsetDecoder}


/**
 *  Functions for bulk handling byte-oriented input.
 */
object ByteInput {

  /**
   *  Read entire file into `Array[Byte]`
   */
  def slurpFile(file: File): Array[Byte] = {
    val fos = new FileInputStream(file)
    try {
      slurpStream(fos)
    } finally { fos.close() }
  }
  
  /**
   *  Read entire contents of `InputStream` into `Array[Byte]`; `InputStream`
   *  not closed
   */
  def slurpStream(is: InputStream): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    try {
      copyStream(is, bos)
      bos.toByteArray
    } finally { bos.close() }
  }

  /**
   *  Write entire contents of `InputStream` to `File`; `InputStream` not closed
   */
  def copyStream(is: InputStream, file: File): Boolean = {
    val os = new FileOutputStream(file)
    try {
      copyStream(is, os)
    } finally { os.close() }
  }

  /**
   *  Write all contents of `InputStream` to `OutputStream`; neither stream
   *  closed
   */
  def copyStream(is: InputStream, os: OutputStream, chunkSize: Int = 4096):
      Boolean = {
    val buff = new Array[Byte](chunkSize)

    def siphonBytes() {
      val n = is.read(buff)
      if (n > 0)
        os.write(buff, 0, n)
      if (n >= 0)
        siphonBytes() // keep siphoning
    }

    siphonBytes()
    true
  }
}


/**
 *  Functions for bulk handling character-oriented input.
 */
object CharInput {

  /**
   *  Read entire text `File` into `String`.
   */
  def slurpTextFile(file: File, charsetName: String = "UTF-8"): String = {
    val fos = new FileInputStream(file)
    try {
      slurpTextStream(fos, charsetName)
    } finally { fos.close() }
  }

  /**
   *  Read entire text `File` into `String`.
   */
  def slurpTextFile(file: File, charset: Charset): String = {
    val fos = new FileInputStream(file)
    try {
      slurpTextStream(fos, charset)
    } finally { fos.close() }
  }

  /**
   *  Read entire text `File` into `String`.
   */
  def slurpTextFile(file: File, charsetDecoder: CharsetDecoder): String = {
    val fos = new FileInputStream(file)
    try {
      slurpTextStream(fos, charsetDecoder)
    } finally { fos.close() }
  }


  /**
   *  Read entire contents of `InputStream` into `String`; `InputStream` not
   *  closed
   */
  def slurpTextStream(is: InputStream, charsetName: String = "UTF-8"):
      String =
    slurpReader(new InputStreamReader(is, charsetName))

  /**
   *  Read entire contents of `InputStream` into `String`; `InputStream` not
   *  closed
   */
  def slurpTextStream(is: InputStream, charset: Charset):
      String =
    slurpReader(new InputStreamReader(is, charset))

  /**
   *  Read entire contents of `InputStream` into `String`; `InputStream` not
   *  closed
   */
  def slurpTextStream(is: InputStream, charsetDecoder: CharsetDecoder):
      String =
    slurpReader(new InputStreamReader(is, charsetDecoder))


  /**
   *  Read entire contents of `Reader` into `String`; `Reader` not closed
   */
  def slurpReader(in: Reader): String = {
    val builder = new StringBuilder
    copyReader(in, builder)
    builder.toString
  }

  /**
   *  Write all contents of `Reader` to `StringBuilder`; `Reader` not closed
   */
  def copyReader(in: Reader, builder: StringBuilder, chunkSize: Int = 4096):
      Boolean = {
    val buff = new Array[Char](chunkSize)

    def siphonBytes() {
      val n = in.read(buff)
      if (n > 0)
        builder.appendAll(buff, 0, n)
      if (n >= 0)
        siphonBytes() // keep siphoning
    }

    siphonBytes()
    true
  }
}
