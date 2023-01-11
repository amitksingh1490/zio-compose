package compose.graphql

import compose.Interpreter
import zio.schema.StandardType
import caliban.Value
import caliban.schema.Step
import sourcecode.Macros.Chunk
import zio.Chunk
import zio.schema.DynamicValue
import zio.query.ZQuery
import zio.schema.meta.MetaSchema

final class StepGenerator(graph: Graph, i: Interpreter) {

  def toValue(input: StandardType[_], result: Any): Value = input match {
    case StandardType.IntType    => Value.IntValue(result.asInstanceOf[Int])
    case StandardType.BoolType   => Value.BooleanValue(result.asInstanceOf[Boolean])
    case StandardType.StringType => Value.StringValue(result.asInstanceOf[String])


    case StandardType.ByteType      => Value.IntValue(result.asInstanceOf[Byte])
    case StandardType.CharType      => Value.StringValue(result.asInstanceOf[Char].toString)
    case StandardType.LongType      => Value.IntValue(result.asInstanceOf[Long])
    case StandardType.BinaryType    => Value.StringValue(result.asInstanceOf[Chunk[Byte]].toString)
    case StandardType.DayOfWeekType =>
      Value.StringValue(result.asInstanceOf[java.time.DayOfWeek].toString)
    case StandardType.ZonedDateTimeType =>
      Value.StringValue(result.asInstanceOf[java.time.ZonedDateTime].toString)
    case StandardType.OffsetTimeType    =>
      Value.StringValue(result.asInstanceOf[java.time.OffsetTime].toString)
    case StandardType.ShortType         => Value.IntValue(result.asInstanceOf[Short])
    case StandardType.LocalTimeType     =>
      Value.StringValue(result.asInstanceOf[java.time.LocalTime].toString)
    case StandardType.YearMonthType     =>
      Value.StringValue(result.asInstanceOf[java.time.YearMonth].toString)
    case StandardType.LocalDateType     =>
      Value.StringValue(result.asInstanceOf[java.time.LocalDate].toString)
    case StandardType.UUIDType => Value.StringValue(result.asInstanceOf[java.util.UUID].toString)
    case StandardType.ZoneOffsetType =>
      Value.StringValue(result.asInstanceOf[java.time.ZoneOffset].toString)
    case StandardType.MonthDayType   =>
      Value.StringValue(result.asInstanceOf[java.time.MonthDay].toString)
    case StandardType.YearType       => Value.IntValue(result.asInstanceOf[java.time.Year].getValue)
    case StandardType.OffsetDateTimeType =>
      Value.StringValue(result.asInstanceOf[java.time.OffsetDateTime].toString)
    case StandardType.BigDecimalType     => Value.FloatValue(result.asInstanceOf[BigDecimal])
    case StandardType.FloatType          => Value.FloatValue(result.asInstanceOf[Float])
    case StandardType.PeriodType         =>
      Value.StringValue(result.asInstanceOf[java.time.Period].toString)
    case StandardType.BigIntegerType     => Value.IntValue(result.asInstanceOf[BigInt])
    case StandardType.UnitType           => Value.NullValue
    case StandardType.MonthType => Value.StringValue(result.asInstanceOf[java.time.Month].toString)
    case StandardType.DurationType      =>
      Value.StringValue(result.asInstanceOf[java.time.Duration].toString)
    case StandardType.ZoneIdType        =>
      Value.StringValue(result.asInstanceOf[java.time.ZoneId].toString)
    case StandardType.InstantType       =>
      Value.StringValue(result.asInstanceOf[java.time.Instant].toString)
    case StandardType.LocalDateTimeType =>
      Value.StringValue(result.asInstanceOf[java.time.LocalDateTime].toString)
    case StandardType.DoubleType        => Value.FloatValue(result.asInstanceOf[Double])
  }

  def resolve(meta: MetaSchema, result: DynamicValue): Step[Any] = {
    meta match {
      case MetaSchema.Product(id, _, fields, _) => Step.ObjectStep(
          id.name,
          fields.map { case (label, meta) =>
            label -> {
              result match {
                case DynamicValue.Record(_, values) => resolve(meta, values(label))
                case result                         => throw new MatchError(result)
              }
            }
          }.toMap,
        )

      case MetaSchema.ListNode(meta, _, _) => result match {
          case DynamicValue.Sequence(chunk) => Step
              .ListStep(chunk.map(result => resolve(meta, result)).toList)
          case result                       => throw new MatchError(result)
        }

      case MetaSchema.Value(valueType, _, _) => result match {
          case DynamicValue.Primitive(result, _) => Step.PureStep(toValue(valueType, result))
          case result                            => throw new MatchError(result)
        }

      // case MetaSchema.Dictionary(keys, values, path, optional) => ???
      // case MetaSchema.Dynamic(withSchema, path, optional)      => ???
      // case MetaSchema.Tuple(path, left, right, optional)       => ???
      // case MetaSchema.Ref(refPath, path, optional)             => ???
      // case MetaSchema.FailNode(message, path, optional)        => ???
      // case MetaSchema.Either(path, left, right, optional)      => ???
      // case MetaSchema.Sum(id, path, cases, optional)           => ???
      case schema                            => throw new MatchError(schema)
    }

  }

  def resolve(graph: Graph): Step[Any] = {

    Step.ObjectStep(
      "Query",
      graph.cons.map(cons =>
        cons.name -> {
          val result = i.evalDynamic(cons.executable, DynamicValue {})
          val query  = ZQuery.fromZIO(result).map(result => resolve(cons.toType.ast, result))
          Step.QueryStep(query)
        },
      ).toMap,
    )
  }

  def resolve: Step[Any] = resolve(graph)
}
