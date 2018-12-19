package thebrains.youradventure.TerminalUIPack

import org.jline.terminal.Size
import thebrains.youradventure.BirdUtils.BirdOperator._

import scala.io.StdIn
import scala.util.Try

class TerminalPrint {

  private def printText(txt: String): Unit = println(txt)

  private def lengthToFarSparce(txt: String): Int = {
    txt.reverse.dropWhile(_ != ' ').length
  }

  private def splitLine(txt: String): List[String] = {
    if (txt.length > TerminalPrint.MaxLineLength) {
      val newLimit = lengthToFarSparce(txt.substring(0, TerminalPrint.MaxLineLength))
      val finalEdge = if (newLimit < TerminalPrint.MaxLineLength / 2) {
        TerminalPrint.MaxLineLength
      } else {
        newLimit
      }
      txt.substring(0, finalEdge) +:
        splitLine(txt.substring(finalEdge))
    } else {
      List(txt)
    }
  }

  /**
    * Protected for test
    */
  protected def previewTextToPrint(txt: Seq[String]): Seq[String] = {
    txt.flatMap(splitLine)
  }

  /**
    * Protected for test
    */
  protected def previewTextToPrint(txt: String): Seq[String] = {
    previewTextToPrint(txt.split(TerminalPrint.NewLineChar))
  }

  private def assemble(txt: Seq[String]): String = {
    txt.mkString(TerminalPrint.NewLineChar)
  }

  def printToConsole(txt: Seq[String]): Unit = {
    txt |> previewTextToPrint |> assemble |> printText
  }

  def printToConsole(txt: String): Unit = {
    txt |> previewTextToPrint |> assemble |> printText
  }

  private def printSameLine(txt: String): Unit = {
    print(txt)
    Console.out.flush()
  }

  def askText(question: String): String = {
    printToConsole(question)
    printSameLine("?> ")
    StdIn.readLine()
  }

  def askInt(question: String): Int = {
    printToConsole(question)
    printSameLine("?> ")
    StdIn.readInt()
  }
}

object TerminalPrint {
  private val TerminalSize: Option[Size] = Try {
    org.jline.terminal.TerminalBuilder
      .builder()
      .system(true)
      .dumb(false)
      .build()
      .getSize
  }.toOption

  private val TerminalWidth:       Option[Int] = TerminalSize.map(_.getColumns)
  private val NewLineChar:         String = "\n"
  private val DefaultTerminalSize: Int = 80
  private val Margin:              Int = 10
  val MaxLineLength:               Int = TerminalWidth.getOrElse(DefaultTerminalSize) - Margin

  def apply(): TerminalPrint = new TerminalPrint()
}
