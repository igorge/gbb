package gie.app.gbb

import net.liftweb.common.LazyLoggable
import scala.slick.session.Session
import java.io.File

class B(){}

class A() {

  def test(v: B): v.type = {
    println("v: "+ (if(v eq null) "null" else v.toString))
    v
  }

}

object test extends LazyLoggable {



  def run(){

//    val a = new A()
//
//    a.test(null)
//    a.test( new B() )



  }


  def run4(){
    println("******************************************************************************************")
    println("******************************************************************************************")
    println("******************************************************************************************")
    model.import_export.export(new File("/tmp/export"))

  }

  def run3(){
    println("******************************************************************************************")
    println("******************************************************************************************")
    println("******************************************************************************************")


    println( model.i2p.getDestHashFromReq() )
  }


  def run2(){

    import scala.concurrent._
    import ExecutionContext.Implicits.global
    import SlickProfile.profile.simple._

    implicit def slickSession:Session = Coordinator.slickSession


    println("******************************************************************************************")
    println("******************************************************************************************")
    println("******************************************************************************************")

    var post1 = new db.FileRecord(None, gie.UUID(), "", None)

    post1 = Coordinator.loanWrapper()( DB.data.insertNewFileRecord(post1) )

    val f1 = future{ try {

      println(">>>>>>>>>>>>>>>>>>>>>>>>>>> f1 before loan")
      Coordinator.loanWrapper(){

      println(">>>>>>>>>>>>>>>>>>>>>>>>>>> f1")

      val q = for( t<-db.FileRecordsTable if t.id === post1.id) yield (t.mime)
      q.update("lalala")
        Thread.sleep(100000)
      println(">>>>>>>>>>>>>>>>>>>>>>>>>>> f1 done")
    } } catch {
      case e:Throwable => e.printStackTrace()
    }
  }

    val f2 = future{ try {

      println(">>>>>>>>>>>>>>>>>>>>>>>>>>> f2 before loan")
      Coordinator.loanWrapper(){
        Thread.sleep(1000)

        println(">>>>>>>>>>>>>>>>>>>>>>>>>>> f2")

        //val q = for( t<-db.FileRecordsTable if t.id === post1.id) yield (t.mime)
        //q.update("lalala")
        //DB.data.getFileRecordByUUID(gie.UUID())
        println(">>>>>>>>>>>>>>>>>>>>>>>>>>> f2 done")
      } } catch {
      case e:Throwable => e.printStackTrace()
    }
    }

    Await.ready(f1,  duration.Duration.Inf)
    Await.ready(f2,  duration.Duration.Inf)



    println("******************************************************************************************")
    println("******************************************************************************************")
    println("******************************************************************************************")


  }

}
