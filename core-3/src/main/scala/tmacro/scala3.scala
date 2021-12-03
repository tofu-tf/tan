package tan
package tmacro

import scala.quoted.*

object scala3 {
  def compileImpl3[Cls: Type, Eff[_]: Type, S: Type](cls: Expr[Cls])(using q: Quotes): Expr[List[S]] = {
    val mirror1 = tmacro.reflector[Cls, Eff](cls)
    val mirror2 = tmacro.postprocessor(mirror1)

    tmacro.generator[Cls, Eff, S](cls, mirror2)
  }
}
