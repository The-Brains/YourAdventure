package thebrains.youradventure.FPTerminalIO

import java.io.IOException

import org.jline.terminal.Size
import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Utils.BirdUtils.BirdOperator._
import thebrains.youradventure.Utils.Error

import scala.util.Try

class TerminalPrint {

  import scalaz.zio.console._

  private def safeFlush: IO[Nothing, Unit] = IO.sync(scala.Console.flush())

  private def printText(txt: String): IO[Nothing, Unit] = putStrLn(txt)

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

  def display(message: TerminalMessage): IO[Error, Input] = {
    for {
      _ <- this.printToConsole(message.messageToShow)
      s <- askQuestion(message)
    } yield {
      s
    }
  }

  private def askQuestion(message: TerminalMessage): IO[Error, Input] = {
    message.question match {
      case Maybe.Just(q) => this.askText(q).map(InputFilled)
      case Maybe.Empty() => IO.sync(InputEmpty)
    }
  }

  def render(g: GameStatus): IO[Error, GameStatus] = {
    for {
      m <- g.getNextMessage
      s <- this.display(m)
      newState <- g.consume(s)
    } yield {
      newState
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

  def printToConsole(txt: Seq[String]): IO[Nothing, Unit] = {
    txt |> previewTextToPrint |> assemble |> printText
  }

  def printToConsole(txt: String): IO[Nothing, Unit] = {
    txt |> previewTextToPrint |> assemble |> printText
  }

  private def printSameLine(txt: String): IO[Nothing, Unit] = {
    for {
      _ <- putStr(txt)
      _ <- safeFlush
    } yield {
      ()
    }
  }

  private def getString: IO[Error, String] = getStrLn.leftMap(e => Error(e))

  private def getInt: IO[Error, Int] = {
    IO.syncCatch(scala.io.StdIn.readInt()) {
      case e: IOException => Error(e)
    }
  }

  private def ask[A](getValue: => IO[Error, A])(
    question: String,
    answer: Maybe[A]
  ): IO[Error, A] = {
    for {
      _ <- printToConsole(question)
      _ <- printSameLine("?> ")
      output <- {
        answer match {
          case Maybe.Just(a) => for {
            _ <- printToConsole(a.toString)
          } yield {
            a
          }
          case Maybe.Empty() => getValue
        }
      }
    } yield {
      output
    }
  }

  def askText(
    question: String,
    answer: Maybe[String] = Maybe.empty
  ): IO[Error, String] = {
    ask(getString)(question, answer)
  }

  def askInt(
    question: String,
    answer: Maybe[Int] = Maybe.empty
  ): IO[Error, Int] = {
    ask(getInt)(question, answer)
  }
}

object TerminalPrint {
  private val Terminal: Option[org.jline.terminal.Terminal] = Try {
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

  private val NewLineChar: String = "\n"
  private val DefaultTerminalSize: Int = 80
  private val Margin: Int = 10

  def maxLineLength: Int = terminalWidth.getOrElse(DefaultTerminalSize) - Margin

  def apply(): TerminalPrint = new TerminalPrint()
}
