package copy

import java.io.{File, FileFilter}

import org.joda.time.DateTime

case class ImageFile(file: File, dateTime: DateTime)

object FileRepository {

  private val workingDirectory = new File(System.getProperty("user.dir"))
  val sourceDir = new File(workingDirectory.getParent, "shared_photos")
  val destDir = new File(workingDirectory.getParent, "sorted_photos")
  val archiveDir = new File(sourceDir, "archive")

  lazy val sourceFiles: Seq[File] = list(sourceDir)

  private object fileFilter extends FileFilter {
    private val validExtension = Seq(".gif", ".jpg", ".png", ".jpeg")

    override def accept(file: File): Boolean =
      (file.isDirectory || validExtension.exists(file.getName.toLowerCase.endsWith)) &&
      file.getName != "archive"

  }

  private def list(dir: File): Seq[File] = {
    dir.listFiles(fileFilter).flatMap{ file =>
      if (file.isDirectory)
        list(file)
      else {
       Seq(file)
      }
    }
  }

  private object DirectoryFilter extends FileFilter {
    override def accept(file: File): Boolean = file.isDirectory && file.getName != "archive"
  }

  private def listDirs(file: File): List[File] = {
    val children = file.listFiles(DirectoryFilter).toList
    if (children.isEmpty) {
      List(file)
    } else {
      children.flatMap(listDirs)
    }
  }

  def cleanupDirectories(): Seq[File] = listDirs(sourceDir).flatMap{ dir =>
    if (dir.listFiles().isEmpty) {
      dir.deleteOnExit()
      Some(dir)
    } else {
      println(s"Not empty $dir")
      None
    }
  }
}
