import sbt._
import Keys._

object ProtoGenerator {

  val gen: Def.Initialize[Task[Seq[File]]] = Def.task {
    val protoFile: File = (sourceDirectory in Compile).value / "proto" / "ql2.proto"
    val output: File = (sourceManaged in Compile).value / "rere" / "ql" / "ql2.scala"

    val lines = IO.readLines(protoFile)
    val data = convertFile(lines)
    IO.write(output, data)
    Seq(output)
  }

  private def convertFile(lines: List[String]): String = {
    val messagePattern = "^\\s*(message|enum) (\\w+) \\{".r
    val valuePattern = "^\\s*(\\w+)\\s*=\\s*(\\w+)".r
    val endRegex = "^\\s*\\}".r

    val indentation = "  "

    val targetPackage = "rere.ql"
    val header = "\n// Automatically generated class\n"
    val rootOpen = "object ql2 {"
    val rootClose = "}"
    val openBlock = "object %s {"
    val valueDefinition = "val %s = %s"
    val closeBlock = "}"

    val builder = new StringBuilder()

    builder.append(s"package $targetPackage\n")
    builder.append(header)
    builder.append(rootOpen)

    val (_, convertedLines) = lines.foldLeft[(Int, List[String])]((1, Nil)) {
      case ((lastIndentLevel, buf), line) =>

        messagePattern.findFirstMatchIn(line).map { openMatch =>
          val str = (indentation * lastIndentLevel) +
            openBlock.format(openMatch.group(2))

          (lastIndentLevel + 1, str :: buf)
        } getOrElse {
          valuePattern.findFirstMatchIn(line).map { valueDefMatch =>
            val str = (indentation * lastIndentLevel) +
              valueDefinition.format(valueDefMatch.group(1), valueDefMatch.group(2))

            (lastIndentLevel, str :: buf)
          } getOrElse {
            endRegex.findFirstMatchIn(line).map { closeMatch =>
              val str = (indentation * (lastIndentLevel - 1)) + closeBlock

              (lastIndentLevel - 1, str :: buf)
            } getOrElse {
              (lastIndentLevel, buf)
            }
          }
        }
    }

    builder.append(convertedLines.reverse.mkString("\n", "\n", "\n"))

    builder.append(rootClose)
    builder.result()
  }
}
