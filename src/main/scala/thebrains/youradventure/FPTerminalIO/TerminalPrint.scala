package thebrains.youradventure.FPTerminalIO

import java.io.IOException

import org.jline.terminal.Size
import scalaz.Maybe
import scalaz.zio.IO
import thebrains.youradventure.Game.GameStatus
import thebrains.youradventure.Utils.BirdUtils.BirdOperator._
import thebrains.youradventure.Utils.Error

import scala.util.Try

class TerminalPrint {

  import scalaz.zio.console._

  private def safeFlush: IO[Nothing, Unit] = IO.sync(scala.Console.flush())

  private def printText(txt: String): IO[Nothing, Unit] = putStrLn(txt)

  private def lengthToFarSparse(txt: String): Int = txt.reverse.dropWhile(_ != ' ').length

  private def splitLine(
    line:             Line,
    ignoreLineLength: Boolean
  ): List[String] = {
    if (ignoreLineLength || !line.cutLength) {
      List(line.content)
    } else {
      val txt = line.content
      if (txt.length > TerminalPrint.maxLineLength) {
        val newLimit = lengthToFarSparse(txt.substring(0, TerminalPrint.maxLineLength))
        val finalEdge = if (newLimit < TerminalPrint.maxLineLength / 2) {
          TerminalPrint.maxLineLength
        } else {
          newLimit
        }
        txt.substring(0, finalEdge) +:
          splitLine(Line(txt.substring(finalEdge)), ignoreLineLength)
      } else {
        List(txt)
      }
    }
  }

  def display(
    message:          MessageToDisplay,
    answer:           Maybe[String] = Maybe.empty,
    ignoreLineLength: Boolean = false
  ): IO[Error, Input] = {
    for {
      _ <- this.printToConsole(message.messageToShow, ignoreLineLength)
      s <- askQuestion(message, answer, ignoreLineLength)
    } yield {
      s
    }
  }

  private def askQuestion(
    message:          MessageToDisplay,
    answer:           Maybe[String] = Maybe.empty,
    ignoreLineLength: Boolean
  ): IO[Error, Input] = {
    message.question match {
      case Maybe.Just(q) => this.askText(q, answer, ignoreLineLength).map(InputFilled)
      case Maybe.Empty() => IO.sync(InputEmpty)
    }
  }

  def render(
    g:      GameStatus,
    answer: Maybe[String] = Maybe.empty
  ): IO[Error, GameStatus] = {
    for {
      m <- g.getNextMessage
      s <- m match {
        case msg: MessageToDisplay => this.display(msg, answer)
        case TerminalMessageBuilder.EmptyMessage => IO.sync(InputEmpty)
      }
      newState <- g.consume(s)
    } yield {
      newState
    }
  }

  /**
    * Protected for test
    */
  protected def previewTextToPrint(
    txt:              Seq[Line],
    ignoreLineLength: Boolean
  ): Seq[String] = {
    txt.flatMap(t => splitLine(t, ignoreLineLength))
  }

  /**
    * Protected for test
    */
  protected def previewTextToPrint(
    txt:              Line,
    ignoreLineLength: Boolean
  ): Seq[String] = {
    previewTextToPrint(
      txt.content.split(TerminalPrint.NewLineChar).map(s => Line(s, txt.cutLength)),
      ignoreLineLength
    )
  }

  private def assemble(txt: Seq[String]): String = {
    txt.mkString(TerminalPrint.NewLineChar)
  }

  private def printToConsole(
    txt:              Seq[Line],
    ignoreLineLength: Boolean
  ): IO[Nothing, Unit] = {
    txt |> (previewTextToPrint(_, ignoreLineLength)) |> assemble |> printText
  }

  private def printToConsole(
    txt:              Line,
    ignoreLineLength: Boolean
  ): IO[Nothing, Unit] = {
    txt |> (previewTextToPrint(_, ignoreLineLength)) |> assemble |> printText
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

  private def ask[A](
    getValue: => IO[Error, A]
  )(
    question:         Line,
    answer:           Maybe[A],
    ignoreLineLength: Boolean
  ): IO[Error, A] = {
    for {
      _ <- printToConsole(question, ignoreLineLength)
      _ <- printSameLine("?> ")
      output <- {
        answer match {
          case Maybe.Just(a) =>
            printToConsole(Line(a.toString), ignoreLineLength)
              .map(_ => a)
          case Maybe.Empty() => getValue
        }
      }
    } yield {
      output
    }
  }

  private def askText(
    question:         Line,
    answer:           Maybe[String] = Maybe.empty,
    ignoreLineLength: Boolean
  ): IO[Error, String] = {
    ask(getString)(question, answer, ignoreLineLength)
  }

  private def askInt(
    question:         Line,
    answer:           Maybe[Int] = Maybe.empty,
    ignoreLineLength: Boolean
  ): IO[Error, Int] = {
    ask(getInt)(question, answer, ignoreLineLength)
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

  private val NewLineChar:         String = "\n"
  private val DefaultTerminalSize: Int = 80
  private val Margin:              Int = 10

  def maxLineLength: Int = terminalWidth.getOrElse(DefaultTerminalSize) - Margin

  def apply(): TerminalPrint = new TerminalPrint()
}
