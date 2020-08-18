package hu.agroferr

import java.io.File

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, Text, XML}

object Main {

  def main(args:Array[String]): Unit = {
    val folderOrFile = new File(args(0))
    folderOrFile.isDirectory match {
      case true =>
        val allFiles = folderOrFile.listFiles()
        allFiles.foreach(file => programRunner(file))
      case false =>
        programRunner(folderOrFile)
    }

  }

  def programRunner(fileName:File): Unit = {
println("Hello Lyanyom: " + fileName.getName)
    //    val invoiceXml = Main.loadXml(fileName)
//    val comments = Main.extractComment(invoiceXml)
//    val invoiceInfo = InvoiceInfo(comments)  //TODO
//    val transformed = Main.transformXml(invoiceXml,invoiceInfo)
//    Main.save(fileName, transformed.head)
  }

  def loadXml(filePath: String): Elem = XML.loadFile(filePath)

  def save(filePath: String, contents: Node): Unit = {
    val dot = filePath.indexOf(".")
    val fileName = filePath.substring(0, dot)
    XML.save(fileName + "_modified.xml", contents)
  }

  def extractComment(invoice: Elem) = {
    val comments = invoice \ "tetelek" \ "tetel" \ "megjegyzes"
    comments.toList.map(_.text).head
  }

  def transformXml(invoice: Elem, invoiceInfo: InvoiceInfo) = {
    val rule1 = getRewriteRule("megrendeles", invoiceInfo.orderNumber)
    val rule2 = getRewriteRule("megrendelesdatum", invoiceInfo.date)
    val rule3 = getRewriteRule("szallitolevel", invoiceInfo.letterNumber)
    val rule4 = getRewriteRule("szallitoleveldatum",invoiceInfo.letterDate)
    val companyName = (invoice \ "fejlec" \ "elado" \ "nev").text
    val rule5 = getRewriteRule("szamlatulaj", companyName)
    val rule6 = getRewriteRule("banknev", "Országos Takarékpénztár és Kereskedelmi Bank")
    object rt1 extends RuleTransformer(rule5,rule6)
    val rule4Seller: RewriteRule = new RewriteRule {
      override def transform(n: Node): collection.Seq[Node] = n match {
        case elem: Elem if elem.label == "elado" => rt1(elem)  //This rule uses another rule
        case other => other
      }
    }

    val transformed = new RuleTransformer(rule1, rule2, rule3, rule4, rule4Seller).transform(invoice)
    transformed
  }


  def getRewriteRule(matchOnLabel:String,newText:String):RewriteRule = {
     new RewriteRule {
      override def transform(n: Node): collection.Seq[Node] = n match {
        case elem: Elem if elem.label == matchOnLabel =>
          Elem(elem.prefix, elem.label, elem.attributes, elem.scope, false, Text(newText))
        case other => other
      }
    }
  }
}


case class InvoiceInfo(orderNumber: String, date: String, letterNumber: String, letterDate: String)

object InvoiceInfo {
  def apply(comment: String): InvoiceInfo = {
    val lines = comment.split("[\r\n]")
    InvoiceInfo(valueParser(lines(0)), transformDates(valueParser(lines(1))), valueParser(lines(2)), transformDates(valueParser(lines(3))))
  }

  def transformDates(in:String):String = {
    val full = in.replaceAll("\\.","- ").trim
    full.take(full.length-1)
  }

  def valueParser(line: String): String = {
    val idx = line.indexOf(":")
    line.substring(idx + 1).trim
  }
}

