package hu.agroferr

import java.io.File

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers._

class MainSpec extends AnyWordSpec with should.Matchers {

//  val fileName = new File("ESzamla_eredeti.xml")
//  Main.transformFile(fileName)

  "setupOutputFolder" should {
    "work on files" in {
      val inputXml = new File("ESzamla_eredeti.xml")
      val outputFolder = Main.setupOutputFolder(inputXml)
      val expectedFolder = (new File("out")).getAbsoluteFile
      outputFolder shouldBe expectedFolder
    }

    "work on folders" in {
      val inputXml = new File(System.getProperty("user.dir")) //(new File(".")).getAbsoluteFile
      val outputFolder = Main.setupOutputFolder(inputXml)
      val expectedFolder = (new File("out")).getAbsoluteFile
      outputFolder shouldBe expectedFolder
    }
  }

  "transformFiles" should {
    "work on single files" in {
      val fileName = "ESzamla_eredeti.xml"
      val inputXml = new File(fileName)
      val outFiles = Main.transformFiles(inputXml)
      val expectedFile = new File(System.getProperty("user.dir") + File.separator + "out" + File.separator + fileName)
      outFiles shouldBe List(expectedFile)
    }

    "work on folders" in {
      val inputXml = new File(System.getProperty("user.dir"))
      val outFiles = Main.transformFiles(inputXml)
      outFiles.size shouldBe 2
    }
  }

  "InvoiceInfo" should {
    "parse" in {
      val comment = """Megrendelés száma: 342590
        |Megrendelés kelte: 2020.08.13.
        |Szállítólevél száma: NYEUR 20/0000572KI
        |Szállítólevél kelte: 2020.08.12.
        |Nettó súly: 8400 kg [2020_0010]
        |Termékkód: 2020_0010
        |Termék egyed:""".stripMargin
      val invoiceInfo = InvoiceInfo(comment)
      invoiceInfo shouldBe InvoiceInfo( "342590", "2020- 08- 13", "NYEUR 20/0000572KI", "2020- 08- 12")
    }
  }
}
