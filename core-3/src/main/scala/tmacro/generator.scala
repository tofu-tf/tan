package tan
package tmacro

import cats.Monad

import tmacro.mirror1.{HttpMethod1, ControllerMirror1, AnnotationMirror1, DefaultBodyMirror1}
import tmacro.mirror2.{ControllerMirror2, MethodInput2, NamingConvention2, PathSegment2}

import scala.quoted.*
import sttp.tapir.*
import sttp.tapir.typelevel.*
import tan.attrs.DefTags

import scala.util.matching.Regex

object generator {
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

  def apply[Cls: Type, F[_]: Type, S: Type](using q: Quotes)(clsExpr: Expr[Cls], mirror: ControllerMirror2[q.reflect.Symbol, q.reflect.TypeRepr]): Expr[List[S]] = {
    import q.reflect._
    import q.reflect.report._

    case class InputWithTree[I <: MethodInput2[TypeRepr]](input: I, expr: Expr[EndpointInput[Unit]])
    case class IndexedInput(input: MethodInput2[TypeRepr], expr: Expr[EndpointInput[Unit]], index: Int)

    val clsSym = TypeRepr.of[Cls].typeSymbol

    val unitSym = TypeRepr.of[Unit].typeSymbol
    val stringSym = TypeRepr.of[String].typeSymbol

    val endpoints = mirror.methods.map { method =>
      val withMethod = method.method match {
        case HttpMethod1.Get => '{ endpoint.get }
        case HttpMethod1.Post => '{ endpoint.post }
        case HttpMethod1.Put => '{ endpoint.put }
        case HttpMethod1.Delete => '{ endpoint.delete }
      }

      val withTags = (mirror.tag, method.tag) match {
        case (Some(gtag), Some(mtag)) => '{ ${ withMethod }.tag(${ Expr(gtag) }).tag(${ Expr(mtag) }) }
        case (Some(gtag), _) => '{ ${ withMethod }.tag(${ Expr(gtag) }) }
        case (_, Some(mtag)) => '{ ${ withMethod }.tag(${ Expr(mtag) }) }
        case _ => withMethod
      }

      val withSummary = {
        val summary = method.summary.getOrElse(rename(method.name, NamingConvention2.SpaceSnake).capitalize)

        '{ ${ withTags }.summary(${ Expr(summary) }) }
      }

      val pathInputs = method.inputs.collect {
        case p: MethodInput2.Path[TypeRepr] => InputWithTree(p, '{ ??? })
      }

      def defBodyToTag(defBody: DefaultBodyMirror1): TypeRepr =
        defBody match {
          case AnnotationMirror1.Json => TypeRepr.of[DefTags.JsonDefTag]
        }

      val nonPathInputs = method.inputs.collect {
        case np: MethodInput2.Query[TypeRepr] =>
          np.tpe.asType match {
            case '[inTpe] =>
              val inExpr = '{
                given Codec[String, inTpe, CodecFormat.TextPlain] = ${
                  Expr.summon[Codec[String, inTpe, CodecFormat.TextPlain]]
                    .getOrElse(errorAndAbort(s"Failed to find string codec for ${np.tpe}"))
                }

                query[inTpe](${ Expr(np.name) }).asInstanceOf[EndpointInput[Unit]]
              }

              InputWithTree(np, inExpr)
          }
        case np: MethodInput2.Body[TypeRepr] =>
          np.tpe.dealias.asType match {
            case dealiased @ '[dealiased] =>
              np.bodyType match {
                case Some(AnnotationMirror1.Json) =>
                  Expr.summon[EndpointInputConstructor[dealiased, DefTags.JsonDefTag]] match {
                    case None => errorAndAbort(s"Failed to find PEI or EIC for type ${np.tpe} (dealiased to $dealiased) (note: this is explicit json body)")
                    case Some(eic) => InputWithTree(np, '{ ${ eic }.instance.asInstanceOf[EndpointInput[Unit]] })
                  }
                case Some(AnnotationMirror1.PlainText) =>
                  if (TypeRepr.of[dealiased].typeSymbol == stringSym) {
                    InputWithTree(np, '{ plainBody[String].asInstanceOf[EndpointInput[Unit]] })
                  } else {
                    errorAndAbort(s"Invalid/unsupported type for plainBody input: $dealiased (expecting String)")
                  }
                case None =>
                  Expr.summon[ProvidedEndpointInput[dealiased]] match {
                    case None => mirror.defaultBody match {
                      case Some(defBody) => defBodyToTag(defBody).asType match {
                        case '[defBody] => Expr.summon[EndpointInputConstructor[dealiased, defBody]] match {
                          case None => errorAndAbort(s"Failed to find PEI or EIC for type ${np.tpe} (dealiased to $dealiased), also tried to check explicit annots")
                          case Some(t) => InputWithTree(np, '{ ${ t }.instance.asInstanceOf[EndpointInput[Unit]] })
                        }
                      }
                      case None => errorAndAbort(s"Failed to find PEI and no defaultBody was found")
                    }
                    case Some(t) => InputWithTree(np, '{ ${ t }.instance.asInstanceOf[EndpointInput[Unit]] })
                  }
              }
          }
      }

      /*

      val secInputs = method.inputs.collect {
        case np: MethodInput2.Security[Type] =>
          c.inferImplicitValue(appliedType(secType.typeConstructor, WildcardType, np.tpe, fTpe)) match {
            case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find Security instance for type ${np.tpe}")
            case sec => InputWithTree(np, sec)
          }
      } */

      if (method.inputs.exists(_.isInstanceOf[MethodInput2.Security[TypeRepr]]))
        errorAndAbort("Security inputs are not supported now")

      val indexedInputs = {
        /* if (secInputs.size > 1) {
          errorAndAbort("Multiple secure endpoints are not supported now")
        } */

        val indexedPathInputs = pathInputs.map { p =>
          IndexedInput(
            input = p.input,
            expr = '{ ??? },
            index = method.pathSegments.collect { case s: PathSegment2.Subst => s }.indexWhere(_.name == p.input.name)
          )
        }

        val indexedNonPathInputs = nonPathInputs.zipWithIndex.map {
          case (np, i) => IndexedInput(np.input, np.expr, i + indexedPathInputs.size)
        }

        /*
        val indexedSecInputs = secInputs.map { np =>
          IndexedInput(np.input, np.tree, -1)
        }*/

        val indexedInputsUnordered = indexedPathInputs /* ++ indexedSecInputs */ ++ indexedNonPathInputs

        method.inputs.map { i => indexedInputsUnordered.find(_.input == i).get }
      }

      val (withPathInputs, arityAfterPath) = method.pathSegments.foldLeft((withSummary, 0)) { (ewa, segment) =>
        segment match {
          case PathSegment2.Raw(name) =>
            val (expr, leftAr) = ewa

            val nextExpr = '{
              given ParamConcat.Aux[Unit, Unit, Unit] = new ParamConcat[Unit, Unit] {
                override type Out = Unit
                override def leftArity = ${ Expr(leftAr) }
                override def rightArity = 0
              }

              ${ expr }.in(${ Expr(name) })
            }

            nextExpr -> leftAr

          case PathSegment2.Subst(name) =>
            val input = pathInputs.find(_.input.name == name).get

            val (expr, leftAr) = ewa

            val rightAr = if (input.input.tpe.typeSymbol == unitSym) 0 else 1

            val nextExpr = input.input.tpe.asType match {
              case '[pathType] => '{
                given Codec[String, pathType, CodecFormat.TextPlain] = ${
                  Expr.summon[Codec[String, pathType, CodecFormat.TextPlain]]
                    .getOrElse(errorAndAbort(s"Failed to find string codec for ${input.input.tpe}"))
                }

                given ParamConcat.Aux[Unit, pathType, Unit] = new ParamConcat[Unit, pathType] {
                  override type Out = Unit
                  override def leftArity = ${ Expr(leftAr) }
                  override def rightArity = ${ Expr(rightAr )}
                }

                ${ expr }.in(path[pathType](${ Expr(input.input.name) }))
              }
            }

            nextExpr -> (leftAr + rightAr)
        }
      }

      val (withNonPathInputs, arityAfterNonPath) = nonPathInputs.foldLeft((withPathInputs, arityAfterPath)) { (ewa, input) =>
        val (expr, leftAr) = ewa

        val rightAr = if (input.input.tpe.typeSymbol == unitSym) 0 else 1

        val nextExpr = '{
          given ParamConcat.Aux[Unit, Unit, Unit] = new ParamConcat[Unit, Unit] {
            override type Out = Unit
            override def leftArity = ${ Expr(leftAr) }
            override def rightArity = ${ Expr(rightAr) }
          }

          ${ expr }.in(${ input.expr })
        }

        nextExpr -> (leftAr + rightAr)
      }

      // todo: security inputs

      def inferOutputTypeEncoder(tpe: TypeRepr): Expr[EndpointOutput[Unit]] =
        tpe.asType match {
          case '[oTpe] =>
            Expr.summon[ProvidedEndpointOutput[oTpe]] match {
              case None => mirror.defaultBody match {
                case Some(defBody) => defBodyToTag(defBody).asType match {
                  case '[defBody] => Expr.summon[EndpointOutputConstructor[oTpe, defBody]] match {
                    case None => errorAndAbort(s"Failed to find PEO or EOC for type $tpe")
                    case Some(t) => '{ ${ t }.instance.asInstanceOf[EndpointOutput[Unit]] }
                  }
                }
              }
              case Some(t) => '{ ${ t }.instance.asInstanceOf[EndpointOutput[Unit]] }
            }
        }

      val withOutputs = method.output.out match {
        case Some(out) => '{
          given ParamConcat.Aux[Unit, Unit, Unit] = new ParamConcat[Unit, Unit] {
            override type Out = Unit
            override def leftArity = 0
            override def rightArity = 1
          }

          ${ withNonPathInputs }.out(${ inferOutputTypeEncoder(out.tpe) })
        }
        case None => withNonPathInputs
      }

      val withErrors = method.output.err match {
        case Some(err) => '{
          given ParamConcat.Aux[Unit, Unit, Unit] = new ParamConcat[Unit, Unit] {
            override type Out = Unit
            override def leftArity = 0
            override def rightArity = 1
          }

          ${ withOutputs }.errorOut(${ inferOutputTypeEncoder(err.tpe) })
        }
        case None => withOutputs
      }

      val realInputType = (pathInputs ++ nonPathInputs)
        .map(_.input.tpe)
        .filter { _.typeSymbol != unitSym }

      val inputTupleClass = defn.TupleClass(realInputType.size)
      val inputTupleType = Applied(TypeIdent(inputTupleClass), realInputType.map(_.asType).map {
        case '[wtf] => TypeTree.of[wtf]
      }).tpe

      val monadF = Expr.summon[Monad[F]].get

      inputTupleType.asType match {
        case '[itt] =>
          val workerLambdaMType = MethodType(List("x"))(_ => List(inputTupleType), _ => TypeRepr.of[F[Either[Unit, Unit]]])
          val workerLambda = Lambda(clsSym, workerLambdaMType, (sym, xs) => {
            val x = xs.head.symbol

            val args = indexedInputs.map {
              case IndexedInput(_, _, ix) =>
                Select.unique(Ref(x), "_" + (ix + 1))
            }

            val call = Apply(Select(clsExpr.asTerm, method.callTarget), args).asExprOf[Any]

            if (method.output.higher) {
              if (method.output.either)
                '{ ${ call }.asInstanceOf[F[Either[Unit, Unit]]] }.asTerm
              else
                '{ ${ monadF }.map(${ call }.asInstanceOf[F[Unit]])(v => Right(v)) }.asTerm
            } else {
              if (method.output.either)
                '{ ${ monadF }.pure(${ call }.asInstanceOf[Either[Unit, Unit]]) }.asTerm
              else
                '{ ${ monadF }.pure(Right(${ call }.asInstanceOf[Unit])) }.asTerm
            }
          }).asExprOf[itt => F[Either[Unit, Unit]]]

          '{ ${ withErrors }.serverLogic[F](${ workerLambda }.asInstanceOf[Unit => F[Either[Unit, Unit]]]).asInstanceOf[S] }
      }
    }

    Expr.ofList(endpoints)
  }
}
