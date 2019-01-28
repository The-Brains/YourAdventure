package thebrains.youradventure.Utils

import scalaz.zio.IO
import thebrains.youradventure.FPTerminalIO.{InputEmpty, InputFilled}
import thebrains.youradventure.FactoriesTest.FGame
import thebrains.youradventure.ParentTest

class JumpIOTest extends ParentTest {
  "JumpIO" - {
    import JumpIO._
    "CMsg" - {
      val g = FGame().extract
      val m = CMsg(InputEmpty, g, passNext = false)

      "ToStable" in {
        val sm = m.toStable
        assertEquals(m.input, sm.input)
        assertEquals(m.game, sm.game)
      }
    }

    "Chain" - {
      val g = FGame().extract
      val i = InputFilled("test")

      "Jump one" in {
        val m = CMsg(i, g, passNext = true)
        val out = IO
          .sync(m)
          .mightJumpNext { _ =>
            fail("That should not have been executed")
          }
          .extract

        assertEquals(i, out.input)
      }

      "Jump two" in {
        val i2 = InputFilled("test 2")
        val m = CMsg(i, g, passNext = false)
        val out = IO
          .sync(m)
          .mightJumpNextChain { msg =>
            assertEquals(i, msg.input)
            IO.sync(CMsg(i2, g, passNext = true))
          }
          .mightJumpNext { _ =>
            fail("That should not happen")
          }.extract

        assertEquals(i2, out.input)
      }

      "No Jump one" in {
        val i2 = InputFilled("test 2")
        val m = CMsg(i, g, passNext = false)
        val out = IO
          .sync(m)
          .mightJumpNext { msg =>
            assertEquals(i, msg.input)
            IO.sync(CMsgS(i2, g))
          }.extract

        assertEquals(i2, out.input)
      }

      "Jump and finish tail" in {
        val i2 = InputFilled("test 2")
        val i3 = InputFilled("test 3")
        val m = CMsg(i, g, passNext = false)
        val out = IO
          .sync(m)
          .mightJumpNextChain { msg =>
            assertEquals(i, msg.input)
            IO.sync(CMsg(i2, g, passNext = false))
          }
          .mightJumpNextChain { msg =>
            assertEquals(i2, msg.input)
            IO.sync(CMsg(i3, g, passNext = true))
          }
          .tailChain { _ =>
            fail("Should not happen")
          }
          .extract

        assertEquals(g, out)
      }

      "Jump and finish active tail" in {
        val i2 = InputFilled("test 2")
        val m = CMsg(i, g, passNext = false)
        val out = IO
          .sync(m)
          .mightJumpNextChain { msg =>
            assertEquals(i, msg.input)
            IO.sync(CMsg(i2, g, passNext = false))
          }
          .tailChain { msg =>
            assertEquals(i2, msg.input)
            IO.sync(msg.game)
          }
          .extract

        assertEquals(g, out)
      }

      "With error" in {
        val m = CMsg(i, g, passNext = false)
        IO.sync(m)
          .mightJumpNextChain { msg =>
            assertEquals(i, msg.input)
            ErrorIO("failed", "this failed")
          }
          .mightJumpNextChain { _ =>
            fail("Should not happen")
          }
          .tailChain { _ =>
            fail("Should not happen")
          }
          .shouldFail
      }
    }
  }
}
