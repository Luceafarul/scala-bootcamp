package example

import sbt.Keys._
import sbt._

object BulkySourcesPlugin extends AutoPlugin {
  // Declare a task
  // taskKey -- method is the description of task that will appear when do 'tasks -V'
  lazy val bulkySources = taskKey[Seq[(Int, File)]](
    "Return Seq of files that line of code more then 'bulkyThresholdInLines'"
  )
  lazy val bulkyThresholdInLines = settingKey[Int](
    "Min bulky threshold lines, files that have more lines is bulky. Default is 100."
  )

  // Then define the task as part of a sequence of settings, called projectSettings
  // This method is called by sbt to find the list of settings
  // and tasks that should be included on sbt projects
  override def projectSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := 100,

    bulkySources := {
      val compileFiles = (sources in Compile).value
      val testCompileFiles = (sources in Test).value
      val result = compileFiles ++ testCompileFiles
      result.map { file =>
        val linesCount = sbt.IO.readLines(file).filterNot(_.isBlank).size
        (linesCount, file)
      }.filter { case (linesCount, _) => linesCount >= bulkyThresholdInLines.value }
    }
  )
}
