package tan
package tmacro

import scala.quoted.*

package object scala3 {
  def compileImpl3[Cls: Type, Eff[_]: Type, S: Type](cls: Expr[Cls])(using q: Quotes): Expr[List[S]] = {
    val mirror1 = reflector[Cls, Eff](cls)
    val mirror2 = postprocessor(mirror1)

    (new generator[q.type])[Cls, Eff, S](cls, mirror2)
  }
}
