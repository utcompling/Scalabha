package org.fiasana.test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.fiasana.X2TXT
import opennlp.scalabha.log.SimpleLogger
import java.io.{StringWriter, BufferedWriter}

class X2TXT_Test extends FlatSpec with ShouldMatchers {
  val fileName = "unitTest"


  val validTree =
    <dataset descriptor="igt">
      <file id="igt_0001" languages="kin,eng">
        <data>
          <unit>
            <align>
              <text langid="kin">
                <s>umugore arabiyi muheera</s>
              </text>
              <text langid="eng">
                <s>The woman is giving it to it for him .</s>
              </text>
            </align>
          </unit>
          <unit>
            <align>
              <text langid="kin">
                <s>umualimu arandika ibaruwa n' ikaramu</s>
              </text>
              <text langid="eng">
                <s>The teacher is writing the letter with a pen .</s>
              </text>
            </align>
          </unit>
        </data>
      </file>
    </dataset>
  val extraLangTree =
    <dataset descriptor="igt">
      <file id="igt_0001" languages="kin,eng">
        <data>
          <unit>
            <align>
              <text langid="kin">
                <s>umugore arabiyi muheera</s>
              </text>
              <text langid="eng">
                <s>The woman is giving it to it for him .</s>
              </text>
              <text langid="eng">
                <s>Again, The woman is giving it to it for him .</s>
              </text>
            </align>
          </unit>
          <unit>
            <align>
              <text langid="kin">
                <s>umualimu arandika ibaruwa n' ikaramu</s>
              </text>
              <text langid="eng">
                <s>The teacher is writing the letter with a pen .</s>
              </text>
            </align>
          </unit>
        </data>
      </file>
    </dataset>
  val missingLangTree =
    <dataset descriptor="igt">
      <file id="igt_0001" languages="kin,eng">
        <data>
          <unit>
            <align>
              <text langid="kin">
                <s>umugore arabiyi muheera</s>
              </text>
            </align>
          </unit>
          <unit>
            <align>
            </align>
          </unit>
        </data>
      </file>
    </dataset>
  val unknownLangTree =
    <dataset descriptor="igt">
      <file id="igt_0001" languages="kin,eng">
        <data>
          <unit>
            <align>
              <text langid="kin">
                <s>umugore arabiyi muheera</s>
              </text>
              <text langid="eng">
                <s>The woman is giving it to it for him .</s>
              </text>
              <text langid="klingon">
                <s>The woman is giving it to it for him .</s>
              </text>
            </align>
          </unit>
          <unit>
            <align>
              <text langid="kin">
                <s>umualimu arandika ibaruwa n' ikaramu</s>
              </text>
              <text langid="eng">
                <s>The teacher is writing the letter with a pen .</s>
              </text>
            </align>
          </unit>
        </data>
      </file>
    </dataset>

  "X2TXT" should "parse a valid tree correctly" in {
    assert(X2TXT(validTree,"unitTest") ===
      Map(("kin"->List("umugore arabiyi muheera <EOS>",
                       "umualimu arandika ibaruwa n' ikaramu <EOS>")),
          ("eng"->List("The woman is giving it to it for him . <EOS>",
                       "The teacher is writing the letter with a pen . <EOS>")))
    )
  }
  "X2TXT" should "complain and ignore on duplicate lang" in {
    val logString = new StringWriter()
    X2TXT.log = new SimpleLogger("UnitTest",SimpleLogger.WARN,new BufferedWriter(logString))

    assert(X2TXT(extraLangTree,"unitTest") ===
      Map(("kin"->List("umugore arabiyi muheera <EOS>",
        "umualimu arandika ibaruwa n' ikaramu <EOS>")),
        ("eng"->List("Again, The woman is giving it to it for him . <EOS>",
          "The teacher is writing the letter with a pen . <EOS>")))
    )
    // this one may not be a good idea. If it causes too much trouble, take it out later.
    assert(logString.toString.replaceAll("\\s+"," ") ===
      "UnitTest: [ERR] In file unitTest, " +
        "there is more than one text node for a language. " +
        "All align nodes must contain a single text node for each language: " +
        "<align> <text langid=\"kin\"> <s>umugore arabiyi muheera</s> " +
        "</text> <text langid=\"eng\"> <s>The woman is giving it to it for him .</s> " +
        "</text> <text langid=\"eng\"> " +
        "<s>Again, The woman is giving it to it for him .</s> </text> </align> "
    )
    assert(X2TXT.log.getStats() === (0,1))
  }
  "X2TXT" should "complain and ignore on missing lang" in {
    val logString = new StringWriter()
    X2TXT.log = new SimpleLogger("UnitTest",SimpleLogger.WARN,new BufferedWriter(logString))
    assert(X2TXT(missingLangTree,"unitTest") ===
      Map(("kin"->List("umugore arabiyi muheera <EOS>",
        "<EOS>")),
        ("eng"->List("<EOS>",
          "<EOS>")))
    )
    // this one may not be a good idea. If it causes too much trouble, take it out later.
    assert(logString.toString.replaceAll("\\s+"," ") ===
      "UnitTest: [ERR] In file unitTest, missing language \"eng\" in the " +
        "following align node. All align nodes must contain a single text " +
        "node for each language: <align> <text langid=\"kin\"> <s>umugore " +
        "arabiyi muheera</s> </text> </align> UnitTest: [ERR] In file " +
        "unitTest, missing languages \"eng,kin\" in the following align node. " +
        "All align nodes must contain a single text node for each language: " +
        "<align> </align> "
    )
    assert(X2TXT.log.getStats() === (0,2))
  }
  "X2TXT" should "complain and ignore on unknown lang (not specified in langs attr)" in {
    val logString = new StringWriter()
    X2TXT.log = new SimpleLogger("UnitTest",SimpleLogger.WARN,new BufferedWriter(logString))
    assert(X2TXT(unknownLangTree,"unitTest") ===
      Map(("kin"->List("umugore arabiyi muheera <EOS>",
        "umualimu arandika ibaruwa n' ikaramu <EOS>")),
        ("eng"->List("The woman is giving it to it for him . <EOS>",
          "The teacher is writing the letter with a pen . <EOS>")))
    )
    // this one may not be a good idea. If it causes too much trouble, take it out later.
    assert(logString.toString.replaceAll("\\s+"," ") ===
      "UnitTest: [ERR] In file unitTest, found unknown language \"klingon\" in align node: <align> " +
        "<text langid=\"kin\"> <s>umugore arabiyi muheera</s> </text> " +
        "<text langid=\"eng\"> <s>The woman is giving it to it for him .</s> </text> " +
        "<text langid=\"klingon\"> <s>The woman is giving it to it for him .</s> </text> </align> "
    )
    assert(X2TXT.log.getStats() === (0,1))
  }
}
