package genovese

import scala.deriving.Mirror
import scala.reflect.ClassTag

trait Featureful[T]:

  def features: IArray[Feature[?]]

  def toFeatures(value: T)(using RuntimeChecks): IArray[NormalisedFloat]

  def fromFeatures(fv: IArray[NormalisedFloat]): T

  def bimap[B](f: T => B, g: B => T) =
    val t = this
    new Featureful[B]:
      override def features: IArray[Feature[?]] = t.features
      def toFeatures(value: B)(using RuntimeChecks): IArray[NormalisedFloat] =
        t.toFeatures(g(value))
      def fromFeatures(fv: IArray[NormalisedFloat]): B = f(t.fromFeatures(fv))
end Featureful

final case class ConstFeatureful[T](const: T) extends Featureful[T]:
  override lazy val features: IArray[Feature[?]]            = IArray.empty
  override def fromFeatures(fv: IArray[NormalisedFloat]): T = const
  override def toFeatures(value: T)(using
      RuntimeChecks
  ): IArray[NormalisedFloat] = IArray.empty

final case class TupleFeatureful[T <: Tuple](values: List[Featureful[?]])
    extends Featureful[T]:
  override lazy val features: IArray[Feature[?]] =
    IArray.from(values.flatMap(_.features))

  override def fromFeatures(fv: IArray[NormalisedFloat]): T =
    val ab = Array.newBuilder[Any]

    var offset = 0

    values.foreach: f =>
      ab += f.fromFeatures(fv.slice(offset, offset + f.features.length))
      offset += f.features.length

    Tuple.fromArray(ab.result()).asInstanceOf[T]
  end fromFeatures

  override def toFeatures(value: T)(using
      RuntimeChecks
  ): IArray[NormalisedFloat] =
    val len = features.length

    val a = Array.fill(len)(NormalisedFloat.ZERO)

    var offset = 0
    values
      .zip(value.toArray)
      .foreach: (f, any) =>
        val iv = f.toFeatures(any.asInstanceOf).unsafeArray
        System.arraycopy(iv, 0, a, offset, f.features.length)
        offset += f.features.length

    IArray.unsafeFromArray(a)
  end toFeatures
end TupleFeatureful

object Featureful:
  def features[T](using f: Featureful[T]) = f.features

  def toFeatures[T](value: T)(using f: Featureful[T], rc: RuntimeChecks) =
    f.toFeatures(value)

  def fromFeatures[T](value: IArray[NormalisedFloat])(using
      f: Featureful[T],
      rc: RuntimeChecks
  ) = f.fromFeatures(value)

  inline def derived[T](using Mirror.Of[T]) = ${
    derivedImpl[T]('{ FieldConfig.None })
  }

  inline def derive[T](inline config: FieldConfig)(using Mirror.Of[T]) =
    ${
      derivedImpl[T]('config)
    }

  inline def deriveTuple[T <: Tuple] = ${ derivedTupleImpl[T] }

  import scala.quoted.*

  private def derivedTupleImpl[T <: Tuple](using Quotes, Type[T]) =
    val inst = Expr.ofList(summonInstances[T])

    '{ TupleFeatureful[T]($inst) }.asExprOf[Featureful[T]]

  private def summonInstances[T: Type](using
      Quotes
  ): List[Expr[Featureful[?]]] =
    Type.of[T] match
      case '[elem *: elems] =>
        '{
          compiletime
            .summonInline[Featureful[elem]]
            .asInstanceOf[Featureful[elem]]
        } :: summonInstances[elems]
      case '[EmptyTuple] => Nil

  private given FromExpr[RuntimeChecks] with
    def unapply(expr: Expr[RuntimeChecks])(using
        Quotes
    ): Option[RuntimeChecks] =
      expr match
        case '{ RuntimeChecks.Full } =>
          Some(RuntimeChecks.Full)
        case '{ RuntimeChecks.None } =>
          Some(RuntimeChecks.None)
        case _ => None
  end given

  private given FromExpr[NormalisedFloat] with
    def unapply(expr: Expr[NormalisedFloat])(using
        Quotes
    ): Option[NormalisedFloat] =
      expr match
        case '{ NormalisedFloat(${ Expr(value) })(using ${ Expr(rc) }) } =>
          given RuntimeChecks = rc
          Some(NormalisedFloat.apply(value)(using rc))
        case _ => None

  private given [T: Type]: ToExpr[Feature[T]] with
    def apply(feature: Feature[T])(using Quotes): Expr[Feature[T]] =
      feature match
        case Feature.FloatRange(from, to) =>
          '{ Feature.FloatRange(${ Expr(from) }, ${ Expr(to) }) }

        case Feature.IntRange(from, to) =>
          '{ Feature.IntRange(${ Expr(from) }, ${ Expr(to) }) }

        case Feature.StringCategory(alts) =>
          '{ Feature.StringCategory(${ Expr(alts) }) }

        case Feature.NormalisedFloatRange =>
          '{ Feature.NormalisedFloatRange }

        case Feature.Bool =>
          '{ Feature.Bool }

        case Feature.Optional[T](feature) =>
          '{ Feature.Optional[T](${ Expr(feature) }) }

        case Feature.IntCategory(alts) =>
          '{ Feature.IntCategory(${ Expr(alts) }) }
  end given

  private given FromExpr[Feature[?]] with
    def unapply(expr: Expr[Feature[?]])(using Quotes): Option[Feature[?]] =
      expr match
        // Match the FloatRange case
        case '{ Feature.FloatRange(${ Expr(from) }, ${ Expr(to) }) } =>
          Some(Feature.FloatRange(from, to).asInstanceOf[Feature[?]])

        case '{ Feature.IntRange(${ Expr(from) }, ${ Expr(to) }) } =>
          Some(Feature.IntRange(from, to).asInstanceOf[Feature[?]])

        case '{ Feature.StringCategory(${ Expr(alts) }) } =>
          Some(Feature.StringCategory(alts).asInstanceOf[Feature[?]])

        case '{ Feature.IntCategory(${ Expr(alts) }) } =>
          Some(Feature.IntCategory(alts).asInstanceOf[Feature[?]])

        // // TODO: how do I implement this?
        // case '{type t; ${CategSummon[t](alts)} } =>
        //  Some(Feature.Categorical[t](alts).asInstanceOf[Feature[?]])

        // Match the NormalisedFloatRange case
        case '{ Feature.NormalisedFloatRange } =>
          Some(Feature.NormalisedFloatRange.asInstanceOf[Feature[?]])

        // Match the Bool case
        case '{ Feature.Bool } =>
          Some(Feature.Bool.asInstanceOf[Feature[?]])

        // If none of the above patterns match, return None
        case _ => None
    end unapply

  end given

  private given FromExpr[FieldConfig] with
    def unapply(expr: Expr[FieldConfig])(using Quotes): Option[FieldConfig] =
      expr match
        // Match the FloatRange case
        case '{ FieldConfig(${ Expr(overrides) }) } =>
          Some(FieldConfig(overrides))

        // If none of the above patterns match, return None
        case _ => None
    end unapply
  end given

  private def derivedImpl[T: Type](cfg: Expr[FieldConfig])(using
      Quotes
  ): Expr[Featureful[T]] =
    import quoted.*
    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    import quotes.reflect.*

    given fc: FieldConfig =
      cfg.valueOrAbort

    val configuredNames = fc.overrides.keySet

    ev match
      case '{
            $m: Mirror.SumOf[T] {
              type MirroredElemTypes = elementTypes
            }
          } =>
        val ct    = Expr.summon[ClassTag[T]].get
        val exprs = List.newBuilder[Expr[T]]

        def traverse[elementTypes: Type]: Unit =
          Type.of[elementTypes] match
            case '[elem *: elems] =>
              val ev: Expr[Mirror.Of[elem]] =
                Expr.summon[Mirror.Of[elem]].getOrElse {
                  report.errorAndAbort(
                    s"Could not find a suitable Mirror for ${TypeRepr.of[elem].show}"
                  )
                }
              ev match
                case '{
                      $mirror: Mirror.Singleton { type MirroredLabel = label }
                    } =>
                  exprs += '{ $mirror.asInstanceOf[T] }
                case '{
                      $mirror: Mirror.SingletonProxy
                    } =>
                  exprs += '{ $mirror.value.asInstanceOf[T] }

              end match

              traverse[elems]
            case _ =>
        end traverse

        traverse[elementTypes]

        val values = Expr.ofList(exprs.result())

        '{
          EnumFeatureful[T](IArray.from($values)(using $ct))
        }

      case '{
            $m: Mirror.ProductOf[T] {
              type MirroredElemTypes  = elementTypes
              type MirroredElemLabels = labels
              type MirroredLabel      = commandName
            }
          } =>
        val caseClassSymbol    = TypeRepr.of[T].typeSymbol
        val primaryConstructor = caseClassSymbol.primaryConstructor
        val paramSymbols       = primaryConstructor.paramSymss.flatten

        val fieldNames = paramSymbols.map(_.name)

        val unknown = (configuredNames.toSet -- fieldNames.toSet).toList.sorted

        if unknown.nonEmpty then
          report.errorAndAbort(s"Unknown field(s): ${unknown.mkString(", ")}")

        val spans = Expr.ofList(fieldOpts[elementTypes](fieldNames))
        val names = Expr(fieldNames)

        '{
          new Featureful[T]:
            private val feats = IArray.from($spans)
            private val _featsLen: IArray[Int] =
              feats.map(_.features.size)
            private val vectorSize = _featsLen.sum

            override lazy val features: IArray[Feature[?]] =
              IArray.from(feats).flatMap(_.features)

            override def fromFeatures(
                fv: IArray[NormalisedFloat]
            ): T =
              var offset = 0

              val ab = Array.newBuilder[Any]

              _featsLen.indices
                .foreach: (idx) =>
                  val len = _featsLen(idx)
                  val sub = fv.slice(offset, offset + len)

                  offset += len

                  ab += feats(idx).fromFeatures(sub)

              $m.fromTuple(Tuple.fromArray(ab.result()).asInstanceOf)

            end fromFeatures

            override def toFeatures(value: T)(using
                RuntimeChecks
            ): IArray[NormalisedFloat] =
              val ab = IArray.newBuilder[NormalisedFloat]
              ab.sizeHint(vectorSize)
              val product = value.asInstanceOf[Product]
              product.productIterator.zipWithIndex.foreach: (any, idx) =>
                ab.addAll(feats(idx).toFeatures(any.asInstanceOf))
              ab.result()
            end toFeatures

            override def toString(): String = "Featureful(" + $names
              .zip(feats)
              .map(_ + "=" + _)
              .mkString(", ") + ")"

        }
    end match
  end derivedImpl

  private def fieldOpts[T: Type](
      fieldNames: List[String]
  )(using Quotes, FieldConfig): List[Expr[Featureful[?]]] =
    (Type.of[T], fieldNames) match
      case ('[elem *: elems], head :: rest) =>
        val span = constructFeature[elem](head)

        span :: fieldOpts[elems](rest)

      case other =>
        Nil
  end fieldOpts

  private def constructFeature[E: Type](
      name: String
  )(using Quotes, FieldConfig): Expr[Featureful[E]] =
    import quotes.reflect.*

    val hasNested = Implicits.search(TypeRepr.of[Featureful[E]]) match
      case res: ImplicitSearchSuccess =>
        Some(res.tree.asExprOf[Featureful[E]])
      case _ => None

    extension [T](e: Expr[Featureful[T]])
      def trustMeBro = e.asExprOf[Featureful[E]]

    val hasOverride = summon[FieldConfig].overrides.get(name)

    Type.of[E] match
      case '[Option[t]] =>
        val span = constructFeature[t](name)
        '{ OptionFeatureful($span) }.trustMeBro

      case '[Boolean] =>
        single('{ Feature.Bool }).trustMeBro

      case '[NormalisedFloat] =>
        single('{ Feature.NormalisedFloatRange }).trustMeBro

      case '[Float] =>
        summon[FieldConfig].overrides.get(name) match
          case None =>
            report.errorAndAbort(
              s"Field config missing for field [$name], which has type Float"
            )
          case Some(Feature.FloatRange(from, to)) =>
            single(Expr(Feature.FloatRange(from, to))).trustMeBro
          case Some(other) =>
            report.errorAndAbort(
              s"Incompatible field config [$other] for field [$name], which has type Float"
            )

      case _ if hasNested.isDefined =>
        hasNested.get

      case '[Map[t, g]] =>
        report.warning(
          s"Field [$name] has type ${TypeRepr.of[Seq[t]].show}, currently genovese cannot handle it. Empty map will be used"
        )

        '{ ConstFeatureful(Map.empty[t, g]) }.trustMeBro
      case '[Seq[t]] =>
        report.warning(
          s"Field [$name] has type ${TypeRepr.of[Seq[t]].show}, currently genovese cannot generated sequences of random length"
        )

        val f = constructFeature[t](name)

        '{ OptionFeatureful[t]($f).bimap(_.toSeq, _.headOption) }.trustMeBro

      case other =>
        hasOverride match
          case None =>
            report.errorAndAbort(
              s"Don't know how to turn field [$name] of type [" + TypeRepr
                .of(using other)
                .show + "] into a feature (or feature vector)"
            )
          case Some(feat) =>
            single[E](Expr(feat.asInstanceOf)).trustMeBro
    end match
  end constructFeature

  private def single[T: Type](f: Expr[Feature[T]])(using
      Quotes
  ): Expr[SingleFeatureful[T]] =
    '{ SingleFeatureful($f) }

end Featureful
