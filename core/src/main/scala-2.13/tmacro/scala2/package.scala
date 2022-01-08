package tan
package tmacro

import scala.reflect.macros.blackbox

package object scala2 {
  def compileImpl2[Cls: c.WeakTypeTag, F[_]](c: blackbox.Context)(cls: c.Expr[Cls])(implicit _f: c.WeakTypeTag[F[_]]): c.Tree = {
    val mirror1 = reflector[Cls, F](c)(cls)
    val mirror2 = postprocessor(mirror1)

    generator[c.type, Cls, F](c)(mirror2)
  }
}
