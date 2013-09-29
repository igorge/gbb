package gie.ut


import gie.raw2xml

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

class raw_to_xml extends FlatSpec with PrivateMethodTester {

  "A parse()" should "parse empty" in {
    val s1 = ""
    raw2xml.parse(s1) should equal (xml.NodeSeq.Empty)
  }

  it should "parse white spaces" in {
    val s1 = " "
    raw2xml.parse(s1).toString() should equal (xml.Unparsed("&nbsp;")toString())
  }

  it should "have working n to br" in {
    val s1 = "1\n2"
     raw2xml.parse(s1).toString() should equal (xml.Unparsed("1<br/>2")toString())
  }

  it should "have working rn to br" in {
    val s1 = "1\r\n2"
    raw2xml.parse(s1).toString() should equal (xml.Unparsed("1<br/>2")toString())
  }

  it should "ignore single r" in {
    val s1 = "1\r2"
    raw2xml.parse(s1).toString() should equal (xml.Unparsed("12")toString())
  }

  it should "parse rn at end and start" in {
    val s1 = "1\r\n"
    raw2xml.parse(s1).toString() should equal (xml.Unparsed("1<br/>")toString())

    raw2xml.parse("\n1").toString() should equal (xml.Unparsed("<br/>1")toString())
    raw2xml.parse("\n 1").toString() should equal (xml.Unparsed("<br/>&nbsp;1")toString())

    raw2xml.parse("1\n").toString() should equal (xml.Unparsed("1<br/>")toString())
    raw2xml.parse("1\n\r\r\r\r").toString() should equal (xml.Unparsed("1<br/>")toString())
  }

  it should "parse test case 1" in {
    val s = "0 ["
    raw2xml.parse(s).toString() should equal (xml.Unparsed("0&nbsp;[")toString())
  }

}




