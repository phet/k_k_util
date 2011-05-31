/*
   file: k_k_/fs.scala

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
package k_k_.fs {

import java.io.File


/**
 *  Functions for deriving a `File` path name from an existing one.
 */
object Derive_Name {

  /**
   *  Returns the file path resulting from adding a directory immediately below
   *  this directory (if `file` exists and is a directory), or else below this
   *  file path's parent directory, and with the same (base)name as `file`.
   */
  def via_subdir(file: File, subdir_name: String): File = {
    if (file.isDirectory)
      new File(file.getPath + File.separatorChar + subdir_name)
    else
      new File(Option(file.getParent).map { _ + File.separator }.getOrElse("") +
               subdir_name + File.separator + file.getName)
  }

  /**
   *  Returns the file path resulting from adding a suffix immediately before
   *  the given file path's trailing extension (if any).
   *
   *  NOTE: only the trailing extension is taken into account (e.g. in
   *        `xyz_file.tar.gz` only `.gz` is considered the trailing extension).
   */
  def via_suffix(file: File, suffix: String): File = {
    val Has_Dot_Ext = "^(.*?)(\\.[^.]*)$".r
    val derived_name = file.getName match {
      case Has_Dot_Ext(base_name, ext) => base_name + suffix + ext
      case no_ext_name => no_ext_name + suffix
    }
    new File(Option(file.getParent).getOrElse("") + File.separator +
             derived_name)
  }
}


/**
 *  Functions to guarantee the file system is in the requisite state.
 */
object Ensure {

  /**
   *  Guarantees the `file`'s parent directory exists, creating it if necessary.
   *
   *  @return    `false` iff parent dir does not exist and could not be created.
   */
  def parent_dir_exists(file: File): Boolean =
    file.isDirectory ||
    file.getParentFile.isDirectory ||
    file.getParentFile.mkdirs
}

}
