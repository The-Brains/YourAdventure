package thebrains.youradventure.TerminalUIPack

import org.jline.terminal.{Size, Terminal}
import scalaz.Maybe
import thebrains.youradventure.BirdUtils.BirdOperator._

import scala.io.StdIn
import scala.util.Try

class TerminalPrint {

  private def printText(txt: String): Unit = println(txt)

  private def lengthToFarSparce(txt: String): Int = {
    txt.reverse.dropWhile(_ != ' ').length
  }

  private def splitLine(txt: String): List[String] = {
    if (txt.length > TerminalPrint.maxLineLength) {
      val newLimit = lengthToFarSparce(txt.substring(0, TerminalPrint.maxLineLength))
      val finalEdge = if (newLimit < TerminalPrint.maxLineLength / 2) {
        TerminalPrint.maxLineLength
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

  def askText(
    question: String,
    answer:   Maybe[String] = Maybe.empty
  ): String = {
    printToConsole(question)
    printSameLine("?> ")
    answer match {
      case Maybe.Just(a) => printToConsole(a); a
      case Maybe.Empty() => StdIn.readLine()
    }
  }

  def askInt(
    question: String,
    answer:   Maybe[Int] = Maybe.empty
  ): Int = {
    printToConsole(question)
    printSameLine("?> ")
    answer match {
      case Maybe.Just(a) => printToConsole(a.toString); a
      case Maybe.Empty() => StdIn.readInt()
    }

  }
}

object TerminalPrint {
  private val Terminal: Option[Terminal] = Try {
    org.jline.terminal.TerminalBuilder
      .builder()
      .system(true)
      .dumb(false)
      .build()
  }.toOption

  private def terminalSize: Option[Size] = {
    Try {
      Terminal.map(_.getSize)
    }.toOption.flatten
  }

  private def terminalWidth: Option[Int] = terminalSize.map(_.getColumns)

  private val NewLineChar:         String = "\n"
  private val DefaultTerminalSize: Int = 80
  private val Margin:              Int = 10

  def maxLineLength: Int = terminalWidth.getOrElse(DefaultTerminalSize) - Margin

  def apply(): TerminalPrint = new TerminalPrint()
}
