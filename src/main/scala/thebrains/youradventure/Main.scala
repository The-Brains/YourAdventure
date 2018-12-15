package thebrains.youradventure

import com.monovore.decline.{Command => Cmd, _}
import cats._
import cats.implicits._
import cats.effect._
import tuco.{Tuco, _}
import Tuco._
import scalaz.Maybe
import scalaz.Maybe.Just
import tuco.shell._

object Main {

  var Stop: scalaz.Maybe[IO[Unit]] = scalaz.Maybe.Empty()
  case class Todo(text: String)

  type TodoState = List[Todo]

  object TodoState {
    val Empty: TodoState = Nil
  }

  type TodoAction = TodoState => SessionIO[TodoState]

  def add(
    index: Int,
    text:  String
  ): TodoAction = { ts =>
    ts.patch(index, List(Todo(text)), 0).pure[SessionIO]
  }

  def deleteAction(index: Int): TodoAction = { ts =>
    if (ts.isDefinedAt(index)) {
      ts.patch(index, Nil, 1).pure[SessionIO]
    } else {
      writeLn(s"No such todo!").as(ts)
    }
  }

  val Clear: TodoAction = ts =>
    readLn("Are you sure (yes/no)? ").map(_.trim.toLowerCase).map {
      case "yes" => Nil
      case _     => ts
    }

  val DestroyAction: TodoAction = ts =>
    readLn("Are you sure (yes/no)? ").map(_.trim.toLowerCase).map {
      case "yes" =>
        Stop.map(_.unsafeRunSync)
        Nil
      case _ => ts
    }

  val ListAction: TodoAction = ts =>
    for {
      cs <- getColumns
      ss = ts.zipWithIndex.map { case (Todo(s), n) => f"${n + 1}%3d. $s".take(cs) }
      _ <- ss.traverse(writeLn)
    } yield {
      ts
    }

  val Ind: Opts[Int] =
    Opts
      .option[Int](
        help = "List index where the todo should appear.",
        short = "i",
        long = "index",
        metavar = "index"
      )
      .withDefault(1)
      .map(_ - 1) // 1-based for the user, 0-based internally

  val Txt: Opts[String] =
    Opts.argument[String](metavar = "\"text\"")

  val AddCommand: Command[SessionIO, Session[TodoState]] = {
    Command("add", "Add a new todo.", (Ind, Txt).mapN(add))
      .zoom(Session.data[TodoState]) // Session.data is a lens
  }

  val DeleteOpt: Opts[TodoAction] = Opts
    .argument[Int](metavar = "index")
    .map(n => deleteAction(n - 1))

  val DeleteCommand: Command[SessionIO, Session[TodoState]] = {
    Command(
      "delete",
      "Delete the specified item.",
      DeleteOpt
    ).zoom(Session.data[List[Todo]])
  }

  val ListCommand: Command[SessionIO, Session[TodoState]] = {
    Command("list", "List the todo items.", ListAction.pure[Opts])
      .zoom(Session.data[List[Todo]])
  }

  val DestroyCommand: Command[SessionIO, Session[TodoState]] = {
    Command("destroy", "Will kill the server", DestroyAction.pure[Opts])
      .zoom(Session.data[List[Todo]])
  }

  val ClearCommand: Command[SessionIO, Session[TodoState]] = {
    Command("clear", "Clears the todo list.", Clear.pure[Opts])
      .zoom(Session.data[List[Todo]])
  }

  val TodoCommands: Commands[TodoState] =
    Commands(AddCommand, DeleteCommand, ListCommand, ClearCommand, DestroyCommand)

  val InitialState: Session[TodoState] =
    Session
      .initial(TodoState.Empty).copy(
        prompt = "todo> ",
        commands = Builtins[TodoState] |+| TodoCommands
      )

  val TodoMain: SessionIO[Unit] =
    for {
      _ <- writeLn("Welcome to TODO!")
      s <- runShell(InitialState)
      _ <- writeLn(s"Exiting with ${s.data.length} item(s) on your list.")
    } yield {
      ()
    }

  def main(args: Array[String]): Unit = {
    //    val hello: SessionIO[Unit] =
    //      for {
    //        _ <- writeLn("Hello World!")
    //        n <- readLn("What is your name? ")
    //        _ <- writeLn(s"Hello $n, and goodbye!")
    //      } yield {
    //        ()
    //      }
    val port = 6666
    //    val conf = Config[IO](hello, port)
    //    val stop = conf.start.unsafeRunSync
    val conf = Config[IO](TodoMain, port)
    Stop = Just(conf.start.unsafeRunSync)
  }
}
