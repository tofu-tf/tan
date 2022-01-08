package tan
package tmacro

import tmacro.mirror2.NamingConvention2

import scala.util.matching.Regex

object renamer {
  private val camelCaseWordBoundary: Regex = "[A-Z\\d]".r

  def rename(name: String, conv: NamingConvention2): String = {
    if (conv == NamingConvention2.AsIs) return name

    val sep = conv match {
      case NamingConvention2.Snake => "_"
      case NamingConvention2.SpaceSnake => " "
      case NamingConvention2.Kebab => "-"
      case _ => "?"
    }

    camelCaseWordBoundary.replaceAllIn(name, sep + _.group(0).toLowerCase())
  }
}
