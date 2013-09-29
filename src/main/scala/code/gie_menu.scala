package gie.lift

import net.liftweb.common.{LazyLoggable, Loggable}
import net.liftweb.http.{SessionVar}

package menu.code.snippet {


  class RenderMenu extends Loggable {

    def render(n: xml.NodeSeq): xml.NodeSeq = {
      if(menu.sessionMenu.set_? && (menu.sessionMenu.is ne null) ) {
        menu.render( menu.sessionMenu.is() )
      } else {
        logger.debug("No menu defined")
        xml.NodeSeq.Empty
      }
    }

  }

}

package object menu extends LazyLoggable {


  val SNIPPET_PATH = "gie.lift.menu.code"


  object sessionMenu extends SessionVar[()=>Panel](null)

  sealed trait MenuElement
  case class MenuItemNull() extends MenuElement
  case class MenuItem(display: xml.NodeSeq, targetUrl: String, var isVisible: Boolean = true, var isActive: ()=>Boolean = ()=>false) extends MenuElement
  case class MenuGroup(var items: Seq[MenuElement], var isVisible: Boolean = true) extends MenuElement
  case class MenuDivider() extends MenuElement
  case class Panel(brand: MenuElement, var items: Seq[MenuElement])

  private def impl_navBar(inner: xml.NodeSeq):xml.NodeSeq =
    <div class="navbar navbar-inverse">
      <div class="navbar-inner">
        <div class="container">
          {impl_navBarCollapseBtn ++ inner}
        </div>
      </div>
    </div>

  private def impl_navBarCollapseBtn =
    <button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
    </button>


  private def impl_navBarBrand(item: MenuElement):xml.NodeSeq = item match {
    case MenuItemNull() => xml.NodeSeq.Empty
    case MenuItem(d, url, isVisible, _) if isVisible =>
      <a class="brand" href={url}> {d} </a>
    case MenuItem(_,_,_, _) => xml.NodeSeq.Empty
  }

  private def impl_navBarProcessMenuElement(me: MenuElement): xml.NodeSeq = me match {

    case MenuGroup(items, isVisible) if isVisible =>
      implNavBarMenuElementsInner( items )

    case MenuItem(d, url, isVisible, isActive) if isVisible =>
      val r = <a href={url}> {d} </a>
      if(isActive())
        <li class="active"> {r} </li>
      else
        <li> {r} </li>

    case MenuDivider()=> <li class="divider-vertical"></li>

    case m:MenuElement =>
      logger.debug("Ignoring: " + m)
      assert(false)
      xml.NodeSeq.Empty
  }


  private def implNavBarMenuElementsInner(elements: Seq[MenuElement]): xml.NodeSeq = elements.foldLeft(xml.NodeSeq.Empty){ _++impl_navBarProcessMenuElement(_) }
  private def implNavBarMenuElements(elements: Seq[MenuElement]): xml.NodeSeq =
    <div class="nav-collapse collapse">
      <ul class="nav"> { implNavBarMenuElementsInner(elements) } </ul>
    </div>



  def render(root: Panel):xml.NodeSeq = {
    val brand = impl_navBarBrand(root.brand)


    impl_navBar( brand ++ implNavBarMenuElements(root.items) )
  }

  def test(){}

}




