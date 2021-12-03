package tan

import scala.reflect.macros.blackbox

package object tmacro {
  def compileImpl[Cls: c.WeakTypeTag, F[_]](c: blackbox.Context)(cls: c.Expr[Cls])(implicit _f: c.WeakTypeTag[F[_]]): c.Tree = {
    val mirror1 = tmacro.reflector[Cls, F](c)(cls)
    val mirror2 = tmacro.postprocessor(mirror1)

    tmacro.generator[c.type, Cls, F](c)(mirror2)
  }
}
