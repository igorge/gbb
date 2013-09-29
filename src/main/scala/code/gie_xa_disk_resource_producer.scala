package gie.xadisk

import gie.util.withRollback
import bitronix.tm.resource.common._
import javax.transaction.xa.XAResource
import org.xadisk.filesystem.standalone.StandaloneFileSystemConfiguration
import net.liftweb.common.LazyLoggable
import org.xadisk.bridge.proxies.interfaces.XAFileSystemProxy
import bitronix.tm.resource.{ResourceRegistrar, ResourceObjectFactory}
import javax.naming.{Reference, StringRefAddr}
import bitronix.tm.internal.XAResourceHolderState
import bitronix.tm.recovery.RecoveryException
import bitronix.tm.BitronixTransactionManager
import java.io.{OutputStream, File}

//////////////////////////////////////////////////////////////////////////////////

class XaDiskXAResourceHolder(private val resource:XAResource, private val bean:ResourceBean) extends AbstractXAResourceHolder {
  import collection.JavaConversions._

  def getXAResource() = resource
  def getResourceBean() = bean
  def close() = throw new UnsupportedOperationException( "XaDiskXAResourceHolder cannot be used with an XAPool" )
  def getConnectionHandle() = throw new UnsupportedOperationException( "XaDiskXAResourceHolder cannot be used with an XAPool" )
  def getLastReleaseDate() = throw new UnsupportedOperationException( "XaDiskXAResourceHolder cannot be used with an XAPool" )
  def getXAResourceHolders() = scala.collection.mutable.Buffer[XAResourceHolder](this)

}

//////////////////////////////////////////////////////////////////////////////////

class XaDiskResourceProducer(tm: BitronixTransactionManager, uniqueName: String, metadataDir: String) extends ResourceBean with XAResourceProducer with LazyLoggable {
  logger.debug("CREATED")
  this.setUniqueName(uniqueName)

  private val resourcesHolders = scala.collection.mutable.Buffer[XaDiskXAResourceHolder]()
  private var recoveryXAResourceHolder:RecoveryXAResourceHolder =  _

  private val configuration = {
    val metaMainRoot = new File(metadataDir)
    val metaRoot = new File(metaMainRoot , uniqueName).toString
    logger.info(s"XADISK metadata root for '${uniqueName}' is at '${metaRoot}'")
    new StandaloneFileSystemConfiguration(metaRoot, uniqueName)
  }
  private val xafs = {
    val xafs = XAFileSystemProxy.bootNativeXAFileSystem(configuration)
    xafs.waitForBootup(-1)
    logger.debug("xadisk ready")
    xafs
  }

  def init(){
    logger.debug("init()")
    ResourceRegistrar.register( this )
  }

  def setFailed(failed: Boolean ){
    logger.debug(s"setFailed(${failed})")
    // XADisk cannot fail as it's not connection oriented ??
  }

  def close() = synchronized {
    logger.debug("close()")
    ResourceRegistrar.unregister( this )
    xafs.shutdown()
  }

  def getReference() = synchronized{
    logger.debug("getReference()")
    new Reference(this.getClass().getName(), new StringRefAddr( "uniqueName", getUniqueName() ), classOf[ResourceObjectFactory].getName(), null )
  }

  def createPooledConnection(factory: AnyRef, bean: ResourceBean):XAStatefulHolder = {
    logger.debug(s"createPooledConnection(${factory}, ${bean}})")
    throw new UnsupportedOperationException( "XaDisk is not connection-oriented" )
  }

  def findXAResourceHolder(resource: XAResource): XAResourceHolder = resourcesHolders.synchronized {
    resourcesHolders.find( _.getXAResource() eq resource).fold[XAResourceHolder]{
      logger.debug(s"findXAResourceHolder(${resource}) returning null (not found)")
      null
    }{v=>
      logger.debug(s"findXAResourceHolder(${resource}) returning ${v} (found)")
      v
    }
  }

  def startRecovery(): XAResourceHolderState = resourcesHolders.synchronized {
    logger.debug("startRecovery()")

    val xaResource = xafs.getXAResourceForRecovery()

    recoveryXAResourceHolder = new RecoveryXAResourceHolder( new XaDiskXAResourceHolder(xaResource, this) )
    new XAResourceHolderState( recoveryXAResourceHolder, this )
  }

  def endRecovery(){
    logger.debug("endRecovery()")
    recoveryXAResourceHolder = null
  }

  private def currentTransaction = tm.getCurrentTransaction()


  def getFs(transactionTimeout: Int) = resourcesHolders.synchronized {
    import javax.transaction.Synchronization

    assume( currentTransaction ne null)

    val xatranses = xafs.createSessionForXATransaction()
    val xaResource = xatranses.getXAResource()
    if( transactionTimeout>0 ) {
      logger.debug(s"Setting XADISK transaction timeout ${transactionTimeout}")
      xaResource.setTransactionTimeout(transactionTimeout)
    }

    val resourceHolder = new XaDiskXAResourceHolder(xaResource, this)

    withRollback{ resourcesHolders += resourceHolder }{ _ => resourcesHolders -= resourceHolder }{ _=>
      withRollback{ currentTransaction.enlistResource( xaResource ) } { _=> currentTransaction.delistResource(xaResource, 0 /*??*/) }{ _=>
        currentTransaction.registerSynchronization( new Synchronization(){
          def beforeCompletion(){}
          def afterCompletion( status:Int ) = resourcesHolders.synchronized {
            logger.debug(s"afterCompletion(${status}})")
            val idx = resourcesHolders.indexOf(resourceHolder)
            if(idx != -1) {
              logger.debug(s"afterCompletion(${status}}), removed: ${resourceHolder} from listed resources")
              resourcesHolders.remove(idx)
            } else {
              logger.info(s"afterCompletion(${status}}), resource ${resourceHolder} not found!")
            }
          }
        })
      }
    }

    xatranses
  }

}

//////////////////////////////////////////////////////////////////////////////////

