package thebrains.youradventure.TerminalUIPack

import thebrains.youradventure.ParentTest

class TerminalPrintTest extends ParentTest {
  "TerminalPrint" - {
    object TerminalPrintForTest extends TerminalPrint {
      override def previewTextToPrint(txt: String): Seq[String] = {
        super.previewTextToPrint(txt)
      }
    }
    "A long text" - {
      val text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque sit " +
        "amet erat nunc. Aliquam semper massa est. Etiam porta accumsan odio, eu ultrices massa " +
        "elementum ac. Maecenas quis sapien ultrices, mollis leo ut, sodales libero. Morbi " +
        "vehicula, nisi at lobortis faucibus, diam urna ultricies tortor, ac porttitor enim " +
        "sapien id neque. Nulla dignissim eget erat sed cursus. Suspendisse rhoncus ultricies" +
        " ligula, congue dictum dui ultricies ut. Maecenas ligula sapien, hendrerit sit amet " +
        "risus euismod, cursus congue enim."

      "should be printed on several lines" in {
        val output = TerminalPrintForTest.previewTextToPrint(text)
        output.foreach(t => assert(t.length <= TerminalPrint.MaxLineLength))
      }
    }

    "A long stream" - {
      def text2: Stream[String] = (0 to 9).map(_.toString).toStream.append(text2)

      "should be printed on several lines" in {
        val lengthToTest = 200
        val output =
          TerminalPrintForTest.previewTextToPrint(text2.take(lengthToTest).toList.mkString(""))
        output.foreach(t => assert(t.length <= TerminalPrint.MaxLineLength))
      }
    }
  }
}
