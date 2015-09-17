package argonaut

import Json._

sealed abstract class CursorOpElement extends Product with Serializable {
  def isLeft: Boolean =
    this == CursorOpLeft

  def isRight: Boolean =
    this == CursorOpRight

  def isFirst: Boolean =
    this == CursorOpFirst

  def isLast: Boolean =
    this == CursorOpLast

  def isUp: Boolean =
    this == CursorOpUp

  def isDeleteGoParent: Boolean =
    this == CursorOpDeleteGoParent

  def isDeleteGoLeft: Boolean =
    this == CursorOpDeleteGoLeft

  def isDeleteGoRight: Boolean =
    this == CursorOpDeleteGoRight

  def isDeleteGoFirst: Boolean =
    this == CursorOpDeleteGoFirst

  def isDeleteGoLast: Boolean =
    this == CursorOpDeleteGoLast

  def isDeleteLefts: Boolean =
    this == CursorOpDeleteLefts

  def isDeleteRights: Boolean =
    this == CursorOpDeleteRights

}
case object CursorOpLeft extends CursorOpElement
case object CursorOpRight extends CursorOpElement
case object CursorOpFirst extends CursorOpElement
case object CursorOpLast extends CursorOpElement
case object CursorOpUp extends CursorOpElement
case class CursorOpLeftN(n: Int) extends CursorOpElement
case class CursorOpRightN(n: Int) extends CursorOpElement
case class CursorOpLeftAt(p: Json => Boolean) extends CursorOpElement
case class CursorOpRightAt(p: Json => Boolean) extends CursorOpElement
case class CursorOpFind(p: Json => Boolean) extends CursorOpElement
case class CursorOpField(f: JsonField) extends CursorOpElement
case class CursorOpDownField(f: JsonField) extends CursorOpElement
case object CursorOpDownArray extends CursorOpElement
case class CursorOpDownAt(p: Json => Boolean) extends CursorOpElement
case class CursorOpDownN(n: Int) extends CursorOpElement
case object CursorOpDeleteGoParent extends CursorOpElement
case object CursorOpDeleteGoLeft extends CursorOpElement
case object CursorOpDeleteGoRight extends CursorOpElement
case object CursorOpDeleteGoFirst extends CursorOpElement
case object CursorOpDeleteGoLast extends CursorOpElement
case class CursorOpDeleteGoField(f: JsonField) extends CursorOpElement
case object CursorOpDeleteLefts extends CursorOpElement
case object CursorOpDeleteRights extends CursorOpElement
case class CursorOpSetLefts(x: List[Json]) extends CursorOpElement
case class CursorOpSetRights(x: List[Json]) extends CursorOpElement

object CursorOpElement extends CursorOpElements

trait CursorOpElements {
}
