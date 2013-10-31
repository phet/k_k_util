/*
   file: k_k_/fs.scala

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
package k_k_.fs {

import java.io.File


/**
 *  Functions for deriving a `File` path name from an existing one.
 */
object DeriveName {

  /**
   *  @return the file path resulting from adding a directory immediately below
   *  this directory (if `file` exists and is a directory), or else below this
   *  file path's parent directory, and with the same (base)name as `file`.
   */
  def viaSubdir(file: File, subdirName: String): File = {
    if (file.isDirectory)
      new File(file.getPath, subdirName)
    else
      new File(Option(file.getParent).map { _ + File.separator }.getOrElse("") +
               subdirName + File.separator + file.getName)
  }

  /**
   *  @return the file path resulting from adding a suffix immediately before
   *  the given file path's trailing extension (if any).
   *
   *  NOTE: only the trailing extension is taken into account (e.g. in
   *        `xyzFile.tar.gz` only `.gz` is considered the trailing extension).
   */
  def viaSuffix(file: File, suffix: String): File = {
    val HasDotExt = "^(.*?)(\\.[^.]*)$".r
    val derivedName = file.getName match {
      case HasDotExt(baseName, ext) => baseName + suffix + ext
      case noExtName => noExtName + suffix
    }
    new File(Option(file.getParent).getOrElse("") + File.separator +derivedName)
  }
}


/**
 *  Functions to guarantee the file system is in the requisite state.
 */
object Ensure {

  /**
   *  Guarantees the `file`'s parent directory exists, creating it if necessary.
   *
   *  @return `false` iff parent dir does not exist and could not be created.
   */
  def parentDirExists(file: File): Boolean =
    file.isDirectory ||
    file.getParentFile.isDirectory ||
    file.getParentFile.mkdirs
}

}
