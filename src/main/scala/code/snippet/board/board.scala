package gie.app.gbb.code.snippet
//////////////////////////////////////////////////////////////////////////////////
import gie.app.gbb.{FS, model, db}

import net.liftweb.common.{Loggable, Box,Empty,Full}
import net.liftweb.util.Helpers._
import net.liftweb.http._

import gie.app.gbb.model
import gie.app.gbb.templates
import net.liftweb.util.CssSel

//////////////////////////////////////////////////////////////////////////////////

class PassThrough {
  def render(in: xml.NodeSeq) = in
}


class AddTopic extends Loggable {

  model.security.requirePrivileges(model.security.privileges.PRIV_VIEW_BOARD)

  lazy val boardId = {
    val b = S.param(model.Board.BOARD_ID).flatMap(asLong(_))
    if( !model.Board.isBoardValid_?(b) ) { model.Board.abort.invalidBoardId(b) }
    b
  }
  lazy val postId = S.param(model.Board.POST_ID).flatMap {postIdStr=>
    val postIdMaybe = asLong(postIdStr)
    if(postIdMaybe.isEmpty) model.Board.abort.invalidPostId(postIdStr)

    postIdMaybe
  }
  lazy val post = postId flatMap { id=>
    val maybePost =  model.Board.getPostById(id)

    if(maybePost.isEmpty || maybePost.get.boardId!=boardId.get){
      model.Board.abort.invalidPostId(postId)
    }

    maybePost
  }

  //
  // Title
  //
  def renderTitle= {
    val board = model.Board.getBoardById(boardId.get).get

     if(postId.isEmpty) {
       "#postNoBlock" #> xml.NodeSeq.Empty &
       "#boardTile" #> board.title &
       "#boardPath" #> board.path
     } else {
       "#boardTitleBlock" #> xml.NodeSeq.Empty &
       "#postNo" #> postId.get
     }
  }

  private def renderPost= {

    if(postId.isDefined) {
      if(post.get.threadId.isDefined) model.Board.abort.rootPostCannotBeAPartOfThread(post.get.id.get)
      "#postRegion" #>  templates.post.bind(post.get, model.FileRecord)
    } else {
      "#postRegion" #> xml.NodeSeq.Empty
    }

  }

  private def validateReplyForm(form: templates.board_post_form.FormData) = {
    true
  }
  private def validateForm(form: templates.board_post_form.FormData) = {
    (!form.comment.isEmpty || !form.email.isEmpty || !form.subject.isEmpty || form.fileHolder.isDefined)
  }


  private def postNewTopic(form: templates.board_post_form.FormData){
    logger.debug("new topic")
    model.security.requirePrivileges(model.security.privileges.PRIV_START_THREAD)


    if(validateForm(form)) {
      var msg = new db.Message(
        boardId = boardId.get,
        subject = form.subject,
        comment = form.comment,
        email   = if (form.email.isEmpty) None else Some(form.email)
      )

      msg = model.Board.postNewTopic(msg, form.fileHolder)
      S.redirectTo(model.Board.urlForPost(msg))
    }

  }

  private def postReply(form: templates.board_post_form.FormData){
    logger.debug("reply")
    model.security.requirePrivileges(model.security.privileges.PRIV_POST_REPLY)


    if(validateReplyForm(form)) {

      var msg = new db.Message(
        boardId = boardId.get,
       // threadId = for( p<-post; id <-p.id ) yield id,
        subject = form.subject,
        comment = form.comment,
        email   = if (form.email.isEmpty) None else Some(form.email)
      )

      msg = model.Board.postReply(post.get, msg, form.fileHolder )

      S.redirectTo(model.Board.urlForBoardWithPost(post.get, msg))
    }
  }


  private def submit(form: templates.board_post_form.FormData) =  {
    logger.debug("submit: " + form)

    if(!postId.isDefined){
      postNewTopic(form)
    } else {
      postReply(form)
    }
  }

  def render = {

    logger.debug(s"Board id: ${boardId}, post id: ${postId}")

    "#postFormId" #> templates.board_post_form.bind(v=>submit(v)) &
    renderPost &
    postId.toOption.fold{boardPosts}{ _=> threadPosts}

  }

  private def boardPosts = {
    //"#threadPosts" #> templates.board.bind(model.Board.getRootPostsByBoard(boardId.get), model.FileRecord)
    "#threadPosts" #> templates.board.bindWithChild(model.Board.getRootPostsByBoardWithChildren(boardId.get), model.FileRecord)
  }

  private def threadPosts = {
    if(post.get.threadId.isDefined) model.Board.abort.rootPostCannotBeAPartOfThread(post.get.id.get)
    "#threadPosts" #> templates.post.bind(model.Board.getPostsByThreadId(postId.get), model.FileRecord)
  }


}

