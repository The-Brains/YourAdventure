package thebrains.youradventure.terminalUI

import scala.io.StdIn

class TerminalPrint {

  def printToConsole(txt: String): Unit = println

  def askText(question: String): String = {
    printToConsole(question)
    StdIn.readLine()
  }

  def askInt(question: String): Int = {
    printToConsole(question)
    StdIn.readInt()
  }
}
