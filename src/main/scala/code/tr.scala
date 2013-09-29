package gie.app.gbb

import java.util.Hashtable
import net.liftweb.util.{LoanWrapper, Props}
import net.liftweb.common.{LazyLoggable, Loggable}
import javax.sql.DataSource
import scala.util.DynamicVariable
import bitronix.tm.BitronixTransaction
import org.xadisk.bridge.proxies.interfaces.XADiskBasicIOOperations
import net.liftweb.http.{LiftRules, S}


object Coordinator extends LazyLoggable { self=>
  import scala.slick.session.Session
  import JTA.tm

  private case class State(
    var slickDyn:Session = null,
    var xaDiskDyn:XADiskBasicIOOperations = null,
    var bitronixDyn:BitronixTransaction = null,  //if null -- no transaction have been started
    var rollback:Boolean = false,
    var transactionTimeOut:Int = 0
  )

  private val stateDyn = new DynamicVariable[State](null)
  private val detectedExceptionDyn = new DynamicVariable[Throwable](null)

  private def impl_definedExceptionHandler(){

    type handlerParams = (net.liftweb.util.Props.RunModes.Value, net.liftweb.http.Req, Throwable)

    LiftRules.exceptionHandler.prepend( new PartialFunction[handlerParams, net.liftweb.http.LiftResponse]{
      def isDefinedAt(x: handlerParams): Boolean = {
        logger.debug(s"Filtering exception '${x._3}")
        detectedExceptionDyn.value = x._3
        false
      }
      def apply(v1: handlerParams): net.liftweb.http.LiftResponse = {
        throw new UnsupportedOperationException
      }
    })
  }

  def boot(){

    impl_definedExceptionHandler()

    LiftRules.early.append{_ => impl_liftHook_startJTATransaction()}
    LiftRules.afterSend.append( (_,_,_,_) =>impl_liftHook_endJTATransaction())

  }

  private def impl_closeSlick(){
    val state = stateDyn.value
    assume(state ne null)

    if(state.slickDyn ne null){
      logger.debug("Closing Slick session")
      state.slickDyn.close()
    }
  }

  private def impl_closeResources(){
    impl_closeSlick()
  }

  private def impl_doCommit(){
    val state = stateDyn.value

    if( (state ne null) && (state.bitronixDyn ne null) ) {

      val currentTransaction = tm.getCurrentTransaction()
      assume(currentTransaction eq state.bitronixDyn)
      if(currentTransaction.timedOut()) {
        logger.error(s"Transaction ${currentTransaction} have timed out! Rolling back")
        tm.rollback()
      } else {

        if(state.rollback){
          logger.debug("Rolling back JTA transaction (signalled by flag)")
          tm.rollback()
        } else {
          logger.debug("Commiting")
          tm.commit()
        }
      }

      impl_closeResources()
    }
  }

  private def impl_doRollback(){
    val state = stateDyn.value

    if( (state ne null) && (state.bitronixDyn ne null) ) {

      val currentTransaction = tm.getCurrentTransaction()
      assume(currentTransaction eq state.bitronixDyn)

      if(currentTransaction.timedOut()){
        logger.error(s"Transaction ${currentTransaction} have timed out! Rolling back")
        tm.rollback()
      } else {
        logger.debug("Rolling back")
        tm.rollback()
      }

      impl_closeResources()
    }
  }

  private def impl_requireState(){
    if( stateDyn.value eq null ){
      stateDyn.value = new State
      logger.debug(s"Created new coordinator's State: ${stateDyn.value} -- JTA transaction scope begin")
    }
  }

  private def isInLoan() = stateDyn.value ne null

  def requireJTA(){
    impl_requireState()
    val state = stateDyn.value

    if( state.bitronixDyn eq null ) {
      assume(isInLoan)
      logger.debug("requireJTA(): creating new session")

      assume(state.xaDiskDyn eq null)
      assume(state.slickDyn eq null)

      assume( tm.getCurrentTransaction eq null )

      if(state.transactionTimeOut>=0) {
        logger.debug(s"requireJTA(): setTransactionTimeout(${state.transactionTimeOut})")
        tm.setTransactionTimeout(state.transactionTimeOut)
      }
      tm.begin()
      state.bitronixDyn = tm.getCurrentTransaction()

    }
  }

  def requireNewTransaction(transactionTimeOut: Int = 0){
    if(isInLoan()){
      logger.debug(s"requireNewTransaction(${transactionTimeOut}): Flushing opened transaction before starting new")
      commit()
    } else {
      logger.debug(s"requireNewTransaction(${transactionTimeOut}): No transaction started, no need to flush")
    }
    if(transactionTimeOut>0){
      impl_requireState()
      logger.debug(s"requireNewTransaction(): setting transaction timeout to ${transactionTimeOut} sec")
      val state = stateDyn.value
      state.transactionTimeOut = transactionTimeOut
    }
  }

  def commit(){
    assume( !impl_isException)
    if(isInLoan()){
      try{
        impl_commitOrRollback()
      } finally { impl_NullifyState()  }
    }
  }

    def doRollbackOnRequestEnd(){
      impl_requireState()
      logger.debug("Setting 'rollback' flag for JTA transaction")
      val state = stateDyn.value
      state.rollback = true
    }

  def requireNewReadOnlyTransaction(transactionTimeOut: Int = 0){
    logger.debug(s"requireNewReadOnlyTransaction(${transactionTimeOut})")
    requireNewTransaction(transactionTimeOut)
    Coordinator.doRollbackOnRequestEnd()

  }

  def requireReadOnlyTransaction(transactionTimeOut: Int = 0){
    logger.debug(s"requireReadOnlyTransaction(${transactionTimeOut})")
    val state = stateDyn.value
    if (state eq null){
      Coordinator.doRollbackOnRequestEnd()
    } else if(state.transactionTimeOut < transactionTimeOut) {
      logger.debug(s"requireReadOnlyTransaction(): Starting new read-only transaction because of timeout requirements: need: ${transactionTimeOut}, effective: ${state.transactionTimeOut}")
      requireNewTransaction(transactionTimeOut)
      Coordinator.doRollbackOnRequestEnd()
    } else {
      logger.debug("requireReadOnlyTransaction(): reusing current transaction")
    }

  }

    def xaDiskSession(): XADiskBasicIOOperations = {
      requireJTA()

      val state = stateDyn.value

      if(state.xaDiskDyn eq null){
        logger.debug("Creating new XADISK session")
        state.xaDiskDyn = BootFSStorage.xaDiskProducer.getFs( state.transactionTimeOut )
      }

      stateDyn.value.xaDiskDyn
    }


    def slickSession: Session = {
      requireJTA()

      if(stateDyn.value.slickDyn eq null){
        logger.debug("Creating new Slick session")
        stateDyn.value.slickDyn = DB.createSession()
      }

      stateDyn.value.slickDyn
    }


  private def impl_exception = detectedExceptionDyn.value
  private def impl_isException = impl_exception ne null
  private def impl_clearExceptionFlag(){
    assume( impl_isException )
    detectedExceptionDyn.value = null
  }


  private def impl_liftHook_startJTATransaction(){
    assume( stateDyn.value eq null)
    logger.debug("Lazy transaction available (2)")
    assume( !impl_isException)
  }

  private def impl_liftHook_endJTATransaction() = if(stateDyn.value ne null) try {
    impl_commitOrRollback()
  } finally {
    impl_NullifyState()
    logger.debug(s"cleanupState() -- JTA transaction scope end")
  }

  private def impl_NullifyState(){
    val state = stateDyn.value
    if(state ne null){
      logger.debug(s"Releasing state: ${stateDyn.value}")
      stateDyn.value = null
    }
  }

  private def impl_commitOrRollback(){
    if( impl_isException ){
      try {
        logger.debug(s"Preparing to rollback because of ${impl_exception}\n ${impl_exception.getStackTraceString}")
        impl_doRollback()
      } finally {
        impl_clearExceptionFlag()
      }
    } else {
      impl_doCommit()
    }
  }

  def loanWrapper[T](transactionTimeOut: Int = 0)(fun: =>T) = {
    requireNewTransaction(transactionTimeOut)
    impl_loanWrapper(fun)
  }

  private def impl_loanWrapper[T](fun: =>T) = try {
    assume( (stateDyn.value eq null) || (stateDyn.value.bitronixDyn eq null) )
    val r = try {
      logger.debug("Lazy transaction available")

      assume( !impl_isException)

      fun

    } catch {
      case e:net.liftweb.http.ResponseShortcutException =>
        assume( !impl_isException)
        impl_doCommit()
        throw e
      case e:Throwable=>
        assume( !impl_isException)
        impl_doRollback
        logger.debug(s"Rolled back due an exception: '${e}'")
        throw e
    }

    impl_commitOrRollback()

    r

  } finally { impl_NullifyState()  }


}

object JTA extends LazyLoggable {
  import bitronix.tm._

  val TX_LOG_PATH = "tm.tx_logs_path"

  impl_configure() // ctor -- do configuration

  def shutdown(){
    intr_mainDBSource.close()
    tm.shutdown()
  }

  private def initDBSource(poolSize:Int = 32) = {
    logger.debug("Main DB datasource init")
    val dbSource = new resource.jdbc.PoolingDataSource()

    { import dbSource._
      setUniqueName("mainDB")
      setClassName("org.h2.jdbcx.JdbcDataSource")
      setMaxPoolSize(poolSize)
      val props = getDriverProperties()
      props.setProperty("URL", DBBoot.connetionUrl)
      dbSource.init()
    }

    dbSource
  }

//  val ctx = {
//    val env = new Hashtable[AnyRef, AnyRef]()
//    env.put(javax.naming.Context.INITIAL_CONTEXT_FACTORY, "bitronix.tm.jndi.BitronixInitialContextFactory")
//
//    new javax.naming.InitialContext(env)
//  }

  val tm =  TransactionManagerServices.getTransactionManager()
  private val intr_mainDBSource = initDBSource()
  assume(intr_mainDBSource  ne null)

  def mainDBSource(): DataSource = intr_mainDBSource

  def currentTransaction = tm.getCurrentTransaction()
  def requireTransaction() {
    assume( tm.getCurrentTransaction ne null )
  }

  def boot() = {
    Some(()=>{this.shutdown()})
  }


  private def impl_configure(){
    logger.debug("Configuring bitronix...")

    val rootDir = Props.get(TX_LOG_PATH).map{new java.io.File(_)}.getOrElse{
      val errMsg = s"${TX_LOG_PATH} not configured to valid path"
      logger.error(errMsg); throw new IllegalArgumentException(errMsg)
    }

    rootDir.mkdirs()
    val conf = TransactionManagerServices.getConfiguration()
    conf.setServerId("gbb-jvm-1")
    conf.setLogPart1Filename(new java.io.File(rootDir, "part1.btm").getPath)
    conf.setLogPart2Filename(new java.io.File(rootDir, "part2.btm").getPath)

    logger.debug("Waiting ...")
    //Thread.sleep(1024)
    logger.debug("... done")
  }


}


