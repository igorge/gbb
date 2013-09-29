package gie.io

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}


trait WithISTR {
  def apply[T](f: InputStream=>T):T
}

case class WithIS(isFun: ()=>InputStream) extends  WithISTR{ outer =>
  def apply[T](f: InputStream=>T) = {
    val is = isFun()
    try {
      f(is)
    } finally {
      is.close
    }
  }

  def map(tfun: InputStream=>InputStream):WithISTR = new WithISTR {
    def apply[T](f: InputStream=>T) =
      outer.apply{originalIs=>
        val transformedIs = tfun(originalIs)
        try{
          f(transformedIs)
        } finally {
          transformedIs.close()
        }
      }
  }

}


object Helpers {

  def withInputStream[T](is: InputStream)(fun: InputStream=>T): T = {
    try {
      fun(is)
    } finally {
      is.close()
    }
  }

  def withOutputStream[T](out: OutputStream)(fun: OutputStream=>T): T = {
    try {
      fun(out)
    } finally {
      out.flush()
      out.close()
    }
  }

  def withFlushedOutputStream[T](out: OutputStream)(fun: OutputStream=>T): T = {
    try {
      fun(out)
    } finally {
      out.flush()
    }
  }


  def copy(in: InputStream, out: OutputStream, bufferSize:Int = 2*1024*1024){
    assume(bufferSize>1)
    val buffer = new Array[Byte](bufferSize)
    Iterator.continually{
      in.read(buffer)
    }.takeWhile(_ != -1).foreach { size => out.write(buffer,0, size) }
  }

  def readAsArray(in: InputStream, bufferSize:Int = 2*1024*1024) = {
    val os = new ByteArrayOutputStream(bufferSize)
    copy(in, os, bufferSize)
    os.toByteArray()
  }

}