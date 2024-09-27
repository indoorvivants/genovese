package genovese

import munit.FunSuite
import org.scalafmt.Scalafmt
import org.scalafmt.config.*
import org.scalafmt.config.Docstrings.*
import java.util.concurrent.ConcurrentHashMap
import org.scalafmt.config.Indents.FewerBraces

given Featureful[Oneline] = Featureful.derive[Oneline](FieldConfig.None)
given Featureful[Wrap]    = Featureful.derive[Wrap](FieldConfig.None)
given Featureful[BlankFirstLine] =
  Featureful.derive[BlankFirstLine](FieldConfig.None)
given Featureful[Style] = Featureful.derive[Style](FieldConfig.None)
given Featureful[Docstrings] = Featureful.derive[Docstrings](
  FieldConfig(Map("wrapMaxColumn" -> Feature.IntRange(0, 1)))
)

given commentsWrap: Featureful[Comments.Wrap] =
  Featureful.derived[Comments.Wrap]
given Featureful[Comments] = Featureful.derived[Comments]
given Featureful[OptIn]    = Featureful.derived[OptIn]

given Featureful[BinPack.Site]        = Featureful.derived[BinPack.Site]
given Featureful[BinPack.ParentCtors] = Featureful.derived[BinPack.ParentCtors]
given Featureful[BinPack] = Featureful.derive[BinPack](
  FieldConfig(
    Map(
      "literalsMinArgCount" -> Feature.IntRange(1, 8),
      "literalsInclude"     -> Feature.StringCategory(List("this")),
      "literalsExclude"     -> Feature.StringCategory(List("a"))
    )
  )
)

given Featureful[FewerBraces] = Featureful.derived[FewerBraces]
given Featureful[Indents.RelativeToLhs] =
  Featureful.derived[Indents.RelativeToLhs]
given Featureful[Indents] = Featureful.derive[Indents](
  FieldConfig(
    Map(
      "main"                         -> Feature.IntCategory(List(0, 2, 4)),
      "significant"                  -> Feature.IntCategory(List(0, 2, 4)),
      "callSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "ctrlSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "binPackCallSite"              -> Feature.IntCategory(List(0, 2, 4)),
      "defnSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "caseSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "matchSite"                    -> Feature.IntCategory(List(0, 2, 4)),
      "ctorSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "extraBeforeOpenParenDefnSite" -> Feature.IntCategory(List(0, 2, 4)),
      "binPackDefnSite"              -> Feature.IntCategory(List(0, 2, 4)),
      "afterInfixSite"               -> Feature.IntCategory(List(0, 2, 4)),
      "withSiteRelativeToExtends"    -> Feature.IntCategory(List(0, 2, 4)),
      "extendSite"                   -> Feature.IntCategory(List(0, 2, 4)),
      "commaSiteRelativeToExtends"   -> Feature.IntCategory(List(0, 2, 4))
    )
  )
)

case class DialectOptions(dialectName: String)

given Featureful[DialectOptions] = Featureful.derive(
  FieldConfig(
    Map(
      "dialectName" -> Feature.StringCategory(List("scala213", "scala3"))
    )
  )
)

type ScalafmtConfigSubset =
  (Docstrings, Comments, Indents, BinPack, OptIn, DialectOptions)

extension (cfg: ScalafmtConfigSubset)
  def toScalafmt(base: ScalafmtConfig): ScalafmtConfig =
    base
      .copy(
        docstrings = cfg._1,
        comments = cfg._2,
        indent = cfg._3,
        binPack = cfg._4,
        optIn = cfg._5
      )
      .withDialect(dialect)

  def dialect = cfg._6.dialectName match
    case "scala213" => scala.meta.dialects.Scala213
    case "scala3"   => scala.meta.dialects.Scala3
end extension

class ScalafmtTest extends FunSuite:
  test("scalafmt"):
    given RuntimeChecks = RuntimeChecks.Full

    val cache = ConcurrentHashMap[ScalafmtConfigSubset, NormalisedFloat]()
    val base  = ScalafmtConfig()

    def fitness(cfg: ScalafmtConfigSubset) =
      import com.github.vickumar1981.stringdistance.*
      import com.github.vickumar1981.stringdistance.StringDistance.*
      import com.github.vickumar1981.stringdistance.impl.ConstantGap

      cache.computeIfAbsent(
        cfg,
        { _ =>
          Scalafmt
            .format(
              text.trim,
              style = cfg.toScalafmt(base)
            )
            .toEither
            .fold(
              exc => NormalisedFloat.ZERO,
              formatted =>
                NormalisedFloat.applyUnsafe(
                  math
                    .abs(NeedlemanWunsch.score(formatted, text, ConstantGap()))
                    .toFloat
                    .max(0.0f)
                    .min(1.0f)
                )
            )

        }
      )
    end fitness

    val trainingConfig = TrainingConfig(
      populationSize = 25,
      mutationRate = NormalisedFloat(0.8f),
      steps = 100,
      random = scala.util.Random(80085L),
      selection = Selection.Top(0.8)
    )

    given Featureful[ScalafmtConfigSubset] =
      Featureful.deriveTuple[ScalafmtConfigSubset]

    object Handler extends EventHandler:
      import TrainingEvent.*, TrainingInstruction.*
      def handle[T](t: TrainingEvent[T], data: T | Null): TrainingInstruction =
        t match
          case TopSpecimen   =>
          case ReportFitness =>
          case _             =>

        Continue
      end handle

    end Handler

    val top = Train(
      featureful = summon[Featureful[ScalafmtConfigSubset]],
      config = trainingConfig,
      fitness = Fitness(fitness),
      events = Handler,
      evaluator = ParallelCollectionsEvaluator
    ).train().maxBy(_._2)._1

end ScalafmtTest

val text =
  """
/** @param oneline
  *       - if fold, try to fold short docstrings into a single line
  *   - if unfold, unfold a single-line docstring into multiple lines
  *   - if keep, preserve the current formatting
  * @param wrap
  *   if yes, allow reformatting/rewrapping the contents of the docstring
  * @param style
  *   - Asterisk: format intermediate lines with an asterisk below the first
  *     asterisk of the first line (aka JavaDoc)
  *   - SpaceAsterisk: format intermediate lines with a space and an asterisk,
  *     both below the two asterisks of the first line
  *   - AsteriskSpace: format intermediate lines with an asterisk and a space,
  *     both below the two asterisks of the first line
  */

  package decline_derive

  import quoted.*

  private[decline_derive] case class CmdHintProvider(e: Expr[Seq[CmdHint]]):
    inline def getHint[T: Type](
        inline f: PartialFunction[CmdHint, T]
    )(using Quotes): Expr[Option[T]] =
      '{ $e.collectFirst(f) }

    end getHint

    def name(using Quotes) =
      getHint:
        case CmdHint.Name(value) => value

    def help(using Quotes) =
      getHint:
        case CmdHint.Help(value) => value

  end CmdHintProvider
"""
