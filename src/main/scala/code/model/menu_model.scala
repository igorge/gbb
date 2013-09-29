package gie.app.gbb.model

import gie.app.gbb.{DB, DBChangeListener}
import net.liftweb.http.{CurrentReq, Req, LiftSession, LiftRules, SessionVar}
import net.liftweb.common.{Full, Empty, LazyLoggable, Box}

import gie.lift.menu
import net.liftweb.util.BasicTypesHelpers.AsLong

object Menu extends LazyLoggable {

  private def impl_checkBoard(valueToCheck: Option[String], id: Long) = valueToCheck match {
    case None => false
    case Some( AsLong(v) ) => v==id
    case Some(_)=> false
  }

  private def menuGen() = {

    val brand = menu.MenuItem( xml.Text("GBB"), "/" )

    val boardGroup = impl_buildBoardMenuGroup()

    val p = menu.Panel( brand, Array(
      impl_buildChatMenuGroup(),
      menu.MenuDivider(),
      boardGroup) )

    ()=>p
  }

  private def impl_buildChatMenuGroup() = menu.MenuGroup{
    menu.MenuItem(
      xml.Text("Chat"),
      Chat.urlPath,
      true,
      () => CurrentReq.value match {
        case r @ Req("chat" :: Nil, suffix, requestType)  => true
        case r @ _ => false
      }
    ) :: Nil
  }

  private def impl_buildBoardMenuGroup() = menu.MenuGroup{
    for( board <- Board.boards ) yield {
      menu.MenuItem(
        xml.Text( board.path ),
        Board.urlPath( board ),
        true,
        ()=>{
          CurrentReq.value match {
            case r @ Req("board" :: Nil, suffix, requestType) if( impl_checkBoard( r.param(Board.BOARD_ID), board.id.get ) ) =>
              logger.debug("Marking menu entry as active because of: " + r)
              true
            case r @ _ =>
              false
          }
        }
      )
    }
  }

  private def impl_setupMenu(){
    val menuGen2: ()=>menu.Panel = ()=>{
      logger.debug("Generating menu")
      val generated = menuGen()
      menu.sessionMenu.atomicUpdate(_=>generated)
      generated()
    }

    menu.sessionMenu.atomicUpdate(_ => menuGen2)
  }

  private def impl_liftHook_afterSessionCreate(session: LiftSession, req: Req){
    logger.debug("Setting up menu")
    impl_setupMenu()
  }


  def boot() = {
    LiftSession.afterSessionCreate = impl_liftHook_afterSessionCreate _ :: LiftSession.afterSessionCreate
    DB.event.boardsUpdated.subscribe(impl_boardsUpdated)
    None
  }


  private object impl_boardsUpdated extends DBChangeListener {
    def update() = {
      logger.debug("Resetting menu " + menu.sessionMenu.is)
      impl_setupMenu()
    }
  }

}