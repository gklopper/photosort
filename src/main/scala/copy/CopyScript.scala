package copy

import java.io.File

import copy.FileRepository.{archiveDir, sourceDir, sourceFiles}
import org.apache.commons.io.FileUtils

object CopyScript extends App {

  val filesWithDates = sourceFiles.map(DatedFile.earliestDate)

  // fixes some unfortunate file name choices
  private def clean(s: String) = s.replace(" ", "_")

  // copy files to sorted directory
  val copiedFiles: Seq[File] = filesWithDates.sortBy(_.date.toDate.getTime)
    .flatMap { photo =>

      val original = photo.file
      val fileName = original.getName
      val datePath = photo.date.toString("/yyyy/MM/dd/")
      val destFile = new File(FileRepository.destDir + datePath + clean(fileName))

      val archiveFile = {
        val relativePath = original.getAbsolutePath.replace(sourceDir.getAbsolutePath, "")
        new File(archiveDir, relativePath)
      }

      if (!destFile.exists()) {
        FileUtils.copyFile(original, destFile, true)
        FileUtils.moveFile(original, archiveFile)

        println(original, archiveFile, destFile)

        Some(destFile)
      } else {
        None
      }
    }


  val deletedDirectories = FileRepository.cleanupDirectories()

  println("--------- NEW FILES --------------")

  copiedFiles.foreach(f => println(f.getAbsolutePath))

  println("----------------------------------")
  println(s"Finished - ${copiedFiles.size} new photos copied")

  println("--------- DELETED DIRECTORIES --------------")

  deletedDirectories.foreach(d => println(d.getAbsolutePath))

  println("----------------------------------")
  println(s"Finished - ${deletedDirectories.size} directories deleted")


}



