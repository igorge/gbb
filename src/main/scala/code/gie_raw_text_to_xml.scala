package gie

import scala.annotation.tailrec

package object raw2xml {

  import gie.cp

  sealed trait State
  case class Start(data: Int) extends State
  case class Eof() extends State
  case class Sym(in: Int) extends State
  case class Space(in: Int, count: Int)  extends State

  case class BrR(in: Int) extends State
  case class BrRN(in: Int) extends State
  case class BrN(in: Int) extends State

  case class StrStart(in: Int) extends State
  case class StrCP(in: Int, accum: java.lang.StringBuilder) extends State
  case class StrEnd(in: Int, accum: java.lang.StringBuilder) extends State

  private def impl_invalidState(state: State): Nothing = throw new IllegalStateException()

  def parse(data: String): xml.NodeSeq = {
    val iter = cp.from(data)
    def feed(): Int = {
      if(iter.hasNext) iter.next() else cp.NULLCP
    }

    val builder = xml.NodeSeq.newBuilder

    def buildWhiteSpace(count: Int){
      val sb = new StringBuilder
      for( i <- (1 to count)) sb.append( "&nbsp;" )

      builder += xml.Unparsed(sb.result())
    }
    def buildEof(){

    }
    def buildBr(){
      builder += <br />
    }
    def buildSingleR(){
    }

    def buildString(accum: java.lang.StringBuilder){
      builder += xml.Text( accum.toString() )
    }

    @tailrec
    def doFsm(state: State):Unit = {
     //println(state)
      state match {

        case Eof() =>
        case Start(in) =>
          doFsm(Sym(in))

        case Sym(cp.NULLCP) =>
          buildEof()
          doFsm(Eof())
        case Sym('\t') => doFsm( Space(feed, 8) )
        case Sym('\r') => doFsm( BrR(feed) )
        case Sym('\n') => doFsm( BrN(feed) )
        case Sym(' ') =>
          doFsm( Space(feed, 1) )
        case Sym(in)   => doFsm( StrStart(in) )

        case StrStart(in) => doFsm( StrCP(in, new java.lang.StringBuilder) )
        case StrCP(' ', accum) => doFsm(StrEnd(' ', accum))
        case StrCP(cp.NULLCP, accum) => doFsm(StrEnd(cp.NULLCP, accum))
        case StrCP('\r', accum) => doFsm(StrEnd('\r', accum))
        case StrCP('\n', accum) => doFsm(StrEnd('\n', accum))
        case StrCP(in, accum) =>
          accum.appendCodePoint(in)
          doFsm(StrCP(feed, accum))
        case StrEnd(in, accum) =>
          buildString(accum)
          doFsm(Sym(in))

        case BrR('\n') => doFsm( BrRN(feed) )
        case BrR(in) =>
          buildSingleR()
          doFsm(Sym(in))
        case BrRN(in) =>
          buildBr()
          doFsm(Sym(in))
        case BrN(in) =>
          buildBr()
          doFsm(Sym(in))


        case Space(' ', count) =>
          doFsm( Space(feed, count+1) )
        case Space(in, count) =>
          buildWhiteSpace(count)
          doFsm(Sym(in))

      }
    }

    doFsm(Start(feed()))



    builder.result()
  }

}

