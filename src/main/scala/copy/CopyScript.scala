package foo

import java.io.File

import com.drew.imaging.ImageMetadataReader
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat.{forPattern => dateFormat}
import org.joda.time.format.DateTimeFormatter

import scala.collection.JavaConversions.iterableAsScalaIterable

object CopyScript extends App {


  private val workingDirectory = new File(System.getProperty("user.dir"))
  private val sourceDir = new File(workingDirectory.getParent, "shared_photos")
  private val destDir = new File(workingDirectory.getParent, "sorted_photos")


  case class ImageFile(file: File, dateTime: DateTime)
  case class DateField(directoryName: String, tagName: String, format: DateTimeFormatter)

  // One of thes image metadata probably gives us the best date
  val dateTags = Seq(
    DateField("Exif IFD0", "Date/Time", dateFormat("yyyy:MM:dd HH:mm:ss")),
    DateField("Exif SubIFD", "Date/Time Original", dateFormat("yyyy:MM:dd HH:mm:ss")),
    DateField("Exif SubIFD", "Date/Time Digitized", dateFormat("yyyy:MM:dd HH:mm:ss"))
  )

  val validTagDirectories = dateTags.map(_.directoryName)
  val validTagNames = dateTags.map(_.tagName)


  val files: Seq[File] = list(sourceDir).filter { file =>
    try {
      ImageMetadataReader.readMetadata(file)
      true
    } catch {
      case e: Exception => false
    }
  }

  private def list(dir: File): Seq[File] = {
    dir.listFiles().toSeq.flatMap{ file =>
      if (file.isDirectory)
        list(file)
      else {
        Seq(file)
      }
    }
  }

  val filesWithDates: Seq[ImageFile] = files.map { file =>

    // get all the tags we care about
    val tags = ImageMetadataReader.readMetadata(file)
      .getDirectories
      .flatMap(_.getTags)
      .toSeq
      .filter(t => validTagDirectories.contains(t.getDirectoryName))
      .filter(t => validTagNames.contains(t.getTagName))

    // parse dates
    val dates = dateTags.flatMap { dateTag =>
      try {
        tags.find(tag => tag.getDirectoryName == dateTag.directoryName && tag.getTagName == dateTag.tagName)
          .map(tag => dateTag.format.parseDateTime(tag.getDescription))
      } catch {
        // there are some weird date/ times (e.g. 90 seconds)
        case e : Exception => Nil
      }
    }.sortBy(_.toDate.getTime)


    // get date from meta data or take last-modified of file
    val date = try {
      dates.head
    } catch {
      case e: Exception =>
        new DateTime(file.lastModified())
    }

    ImageFile(file, date)
  }

  // fixes some unfortunate file name choices
  private def clean(s: String) = s.replace(" ", "_")

  // copy files to sorted directory
  val copiedFiles: Seq[File] = filesWithDates.sortBy(_.dateTime.toDate.getTime)
    .flatMap { photo =>

      val original = photo.file
      val fileName = original.getName
      val datePath = photo.dateTime.toString("/yyyy/MM/dd/")
      val destFile = new File(destDir + datePath + clean(fileName))

      if (!destFile.exists()) {
        FileUtils.copyFile(original, destFile, true)

        println(destFile)

        Some(destFile)
      } else {
        None
      }
    }

  println("--------- NEW FILES --------------")

  copiedFiles.foreach(f => println(f.getAbsolutePath))

  println("----------------------------------")
  println(s"Finished - ${copiedFiles.size} new photos copied")


}



