package hu.agroferr

import java.io.File

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers._

class MainSpec extends AnyWordSpec with should.Matchers {

  "This" should {
    "work" in {
      val fileName = new File("ESzamla_eredeti.xml")
      val invoiceXml = Main.loadXml(fileName)
      val comments = Main.extractComment(invoiceXml)
      val invoiceInfo = InvoiceInfo(comments)  //TODO
      val transformed = Main.transformXml(invoiceXml,invoiceInfo)
      Main.save(fileName, transformed.head)
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
