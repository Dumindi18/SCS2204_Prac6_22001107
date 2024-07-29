import scala.io.StdIn
import scala.util.Try

object Q2 {

  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
  }


  def getStudentInfo(name: String, marks: Int, totalMarks: Int): (String, Int, Int, Double, Char) = {
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
    (name, marks, totalMarks, percentage, grade)
  }


  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks: $marks / $totalMarks")
    println(s"Percentage: $percentage%")
    println(s"Grade: $grade")
  }


  def validateInput(name: String, marks: String, totalMarks: String): (Boolean, Option[String]) = {
    if (name.trim.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else {
      (Try(marks.toInt), Try(totalMarks.toInt)) match {
        case (scala.util.Success(m), scala.util.Success(t)) if m >= 0 && m <= t =>
          (true, None)
        case (scala.util.Success(_), scala.util.Success(_)) =>
          (false, Some("Marks should be between 0 and total possible marks."))
        case _ =>
          (false, Some("Invalid input. Please enter positive integers for marks and total possible marks."))
      }
    }
  }


  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var valid = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')

    while (!valid) {
      println("Enter student name:")
      val name = StdIn.readLine()

      println("Enter marks obtained:")
      val marks = StdIn.readLine()

      println("Enter total possible marks:")
      val totalMarks = StdIn.readLine()

      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)

      if (isValid) {
        val marksInt = marks.toInt
        val totalMarksInt = totalMarks.toInt
        studentInfo = getStudentInfo(name, marksInt, totalMarksInt)
        valid = true
      } else {
        println(s"Error: ${errorMessage.getOrElse("Invalid input.")}")
      }
    }
    studentInfo
  }
}
