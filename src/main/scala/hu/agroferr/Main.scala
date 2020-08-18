package hu.agroferr

import java.io.File

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, Text, XML}

object Main {


  def loadXml(filePath: String): Elem = XML.loadFile(filePath)

  def save(filePath:String, contents:Node): Unit = {
    val dot = filePath.indexOf(".")
    val fileName = filePath.substring(0,dot)
    XML.save(fileName+"_modified.xml",contents)
  }
  def extractComment(invoice: Elem) = {
    val comments = invoice \ "tetelek" \ "tetel" \ "megjegyzes"
    comments.toList.map(_.text).headOption
  }

  def transformXml(invoice: Elem, invoiceInfo: InvoiceInfo) = {
    val rule1:RewriteRule = new RewriteRule {
      override def transform(n: Node): collection.Seq[Node] = n match {
        case elem:Elem if elem.label == "megrendeles" =>
          Elem(elem.prefix,elem.label,elem.attributes,elem.scope,false, Text(invoiceInfo.orderNumber))
        case other => other
      }
    }
    val rule2:RewriteRule = new RewriteRule {
      override def transform(n: Node): collection.Seq[Node] = n match {
        case elem:Elem if elem.label == "megrendelesdatum" =>
          Elem(elem.prefix,elem.label,elem.attributes,elem.scope,false, Text(invoiceInfo.date))
        case other => other
      }
    }
    val rule3:RewriteRule = new RewriteRule {
      override def transform(n: Node): collection.Seq[Node] = n match {
        case elem:Elem if elem.label == "szallitolevel" =>
          Elem(elem.prefix,elem.label,elem.attributes,elem.scope,false, Text(invoiceInfo.letterNumber))
        case other => other
      }
    }
    val rule4:RewriteRule = new RewriteRule {
      override def transform(n: Node): collection.Seq[Node] = n match {
        case elem:Elem if elem.label == "szallitoleveldatum" =>
          Elem(elem.prefix,elem.label,elem.attributes,elem.scope,false, Text(invoiceInfo.letterDate))
        case other => other
      }
    }
    val transformed = new RuleTransformer(rule1,rule2,rule3,rule4).transform(invoice)
    transformed
  }
}


case class InvoiceInfo(orderNumber:String,date:String,letterNumber:String,letterDate:String)
object InvoiceInfo {
  def apply(comment:String):InvoiceInfo = {
      val lines = comment.split("[\r\n]")
      InvoiceInfo(valueParser(lines(0)),valueParser(lines(1)),valueParser(lines(2)),valueParser(lines(3)))
  }

  def valueParser(line:String):String = {
    val idx = line.indexOf(":")
    line.substring(idx+1).trim
  }
}