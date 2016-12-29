package copy

import java.io.File

import scala.collection.JavaConversions._
import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.Tag
import org.joda.time.{DateTime, LocalDate}
import org.joda.time.format.DateTimeFormat.{forPattern => dateFormat}



case class DatedFile(file: File, date: LocalDate)

object DatedFile {

  private case class DateField(directoryName: String, tagName: String)

  private val format = dateFormat("yyyy:MM:dd")

  // One of thes image metadata probably gives us the best date
  private val dateTags = Seq(
    DateField("Exif IFD0", "Date/Time"),
    DateField("Exif SubIFD", "Date/Time Original"),
    DateField("Exif SubIFD", "Date/Time Digitized")
  )

  private val validTagDirectories = dateTags.map(_.directoryName)
  private val validTagNames = dateTags.map(_.tagName)


  def earliestDate(file: File): DatedFile = {
    val tags: Seq[Tag] = try {
      ImageMetadataReader.readMetadata(file).getDirectories.flatMap(_.getTags).toSeq
    } catch {
      case e: Exception =>
        println(s"Error parsing file: $file")
        throw e
    }

    val dateFromMetadata = tags
      .filter(tag => validTagDirectories.contains(tag.getDirectoryName))
      .filter(tag => validTagNames.contains(tag.getTagName))
      .map { tag =>
        // the date is in format yyyy:MM:dd HH:mm:ss and we are only interested in the day
        val date = format.parseLocalDate(tag.getDescription.split(" ").head)
        DatedFile(file, date)
      }

    val allDates = dateFromMetadata :+ DatedFile(file, new LocalDate(file.lastModified()))

    //get the earliest date
    allDates.sortBy(_.date.toDateTimeAtStartOfDay.getMillis).head
  }

}