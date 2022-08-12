package compose

import zio.schema.ast.SchemaAst
import zio.schema.{DynamicValue, Schema}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import zio.{Ref, Task, UIO, ZIO}

final case class Interpreter(scope: Interpreter.Scope[Int, Int, DynamicValue]) {
  import Interpreter._

  def eval(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.EndScope(id) =>
        scope.deleteScope(id).as(input)

      case ExecutionPlan.Debug(name, plan) =>
        for {
          result <- eval(plan, input)
          _      <- ZIO.succeed(println(s"${name}: ${input} ~> ${result}"))
        } yield result

      case ExecutionPlan.Arg(i, a1, a2) =>
        val s1 = a1.toSchema.asInstanceOf[Schema[Any]]
        val s2 = a2.toSchema.asInstanceOf[Schema[Any]]

        for {
          value  <- effect(input.toTypedValue(Schema.tuple2(s1, s2)))
          result <- i match {
            case 0 => ZIO.succeed(encode(value._1)(s1))
            case 1 => ZIO.succeed(encode(value._2)(s2))
            case n =>
              ZIO.fail(
                new RuntimeException(s"Can not extract element at index ${i} from ${value.getClass().getName()}"),
              )
          }
        } yield result

      case ExecutionPlan.GetScope(scopeId, ctxId, initial, ast) =>
        val schema = ast.toSchema.asInstanceOf[Schema[Any]]
        for {
          option <- scope.get(scopeId, ctxId)
          value  <- option match {
            case Some(value) => ZIO.succeed(value)
            case None        => ZIO.succeed(initial)
          }
        } yield value

      case ExecutionPlan.SetScope(scopeId, ctxId) =>
        for {
          _ <- scope.set(scopeId, ctxId, input)
        } yield encode(())

      case ExecutionPlan.RepeatUntil(f, cond) =>
        def loop: Task[DynamicValue] = for {
          output <- eval(f, input)
          isTrue <- evalTyped[Boolean](cond, input)
          result <- if (isTrue) ZIO.succeed(output) else loop
        } yield result
        loop

      case ExecutionPlan.Concat(self, other, canConcat) =>
        for {
          canConcat <- effect(canConcat.toTypedValue(Schema[CanConcat[_]]))
          result    <- canConcat match {
            case CanConcat.ConcatString =>
              evalTyped[String](self, input).zipWithPar(evalTyped[String](other, input)) { case (a, b) =>
                encode(a + b)
              }
          }
        } yield result

      case ExecutionPlan.Default(value) => ZIO.succeed(value)

      case ExecutionPlan.SetPath(path) =>
        input match {
          case DynamicValue.Tuple(DynamicValue.Record(values), input) =>
            def loop(
              path: List[String],
              values: ListMap[String, DynamicValue],
              a: DynamicValue,
            ): Either[Exception, DynamicValue] = {
              path match {
                case Nil          => Left(new Exception("Path not found"))
                case head :: tail =>
                  values.get(head) match {
                    case None    => Left(new Exception("Path not found"))
                    case Some(v) =>
                      if (tail.isEmpty) Right(DynamicValue.Record(values + (head -> a)))
                      else
                        loop(tail, v.asInstanceOf[DynamicValue.Record].values, a) map { value =>
                          DynamicValue.Record(values + (head -> value))
                        }
                  }
              }
            }
            ZIO.fromEither(loop(path, values, input))
          case input => ZIO.fail(new Exception(s"Set path doesn't work on: ${input}"))
        }

      case ExecutionPlan.LogicalAnd(left, right) =>
        for {
          left  <- evalTyped[Boolean](left, input)
          right <- evalTyped[Boolean](right, input)
        } yield encode { left && right }

      case ExecutionPlan.LogicalOr(left, right) =>
        for {
          left  <- evalTyped[Boolean](left, input)
          right <- evalTyped[Boolean](right, input)
        } yield encode { left || right }

      case ExecutionPlan.LogicalNot(plan)                             =>
        for {
          bool <- evalTyped[Boolean](plan, input)
        } yield encode { !bool }
      case ExecutionPlan.NumericOperation(operation, left, right, is) =>
        for {
          isNumeric <- effect(is.toTypedValue(Schema[IsNumeric[_]]))
          params    <-
            isNumeric match {
              case IsNumeric.NumericInt =>
                evalTyped[Int](left, input).zip(evalTyped[Int](right, input)).map { case (a, b) =>
                  operation match {
                    case Numeric.Operation.Add                => a + b
                    case Numeric.Operation.Multiply           => a * b
                    case Numeric.Operation.Subtract           => a - b
                    case Numeric.Operation.Divide             => a / b
                    case Numeric.Operation.GreaterThan        => if (a > b) 1 else 0
                    case Numeric.Operation.GreaterThanEqualTo => if (a >= b) 1 else 0
                  }
                }
            }
        } yield encode(params)

      case ExecutionPlan.Combine(left, right, o1, o2) =>
        eval(left, input).zip(eval(right, input)).flatMap { case (a, b) =>
          effect(merge(a, b, o1, o2))
        }

      case ExecutionPlan.IfElse(cond, ifTrue, ifFalse) =>
        for {
          cond   <- evalTyped[Boolean](cond, input)
          result <- if (cond) eval(ifTrue, input) else eval(ifFalse, input)
        } yield result
      case ExecutionPlan.Pipe(first, second)           =>
        for {
          input  <- eval(first, input)
          output <- eval(second, input)
        } yield output
      case ExecutionPlan.GetPath(path)                 =>
        input match {
          case DynamicValue.Record(values) =>
            @tailrec
            def loop(path: List[String], values: ListMap[String, DynamicValue]): Either[Exception, DynamicValue] = {
              path match {
                case Nil          => Left(new Exception("Path not found"))
                case head :: tail =>
                  values.get(head) match {
                    case None    => Left(new Exception("Path not found"))
                    case Some(v) =>
                      if (tail.isEmpty) Right(v)
                      else
                        loop(tail, v.asInstanceOf[DynamicValue.Record].values)
                  }
              }
            }
            ZIO.fromEither(loop(path, values))
          case _                           => ZIO.fail(new Exception("Select only works on records"))
        }
      case ExecutionPlan.Equals(left, right)           =>
        for {
          left  <- eval(left, input)
          right <- eval(right, input)
        } yield encode(left == right)

      case ExecutionPlan.FromMap(value)  =>
        value.get(input) match {
          case Some(v) => ZIO.succeed(v)
          case None    =>
            ZIO.fail(new Exception("Key lookup failed in dictionary"))
        }
      case ExecutionPlan.Constant(value) => ZIO.succeed(value)
      case ExecutionPlan.Identity        => ZIO.succeed(input)
    }
  }

  def evalDynamic(plan: ExecutionPlan, value: DynamicValue): Task[DynamicValue] =
    eval(plan, value)

  def evalTyped[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    evalDynamic(plan, value).flatMap(value => effect(value.toTypedValue(ev)))
}
object Interpreter                                                             {

  def evalDynamic(plan: ExecutionPlan, value: DynamicValue): Task[DynamicValue] =
    make.flatMap(i => i.evalDynamic(plan, value))

  def evalDynamic[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[DynamicValue] =
    evalDynamic(lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalTyped[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[B] =
    evalTyped[B](lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalTyped[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    make.flatMap(i => i.evalTyped(plan, value))

  def make: UIO[Interpreter] =
    Scope.make[Int, Int, DynamicValue].map(scope => new Interpreter(scope))

  private def effect[A](e: Either[String, A]): Task[A] =
    e match {
      case Left(error) => ZIO.fail(new Exception(error))
      case Right(a)    => ZIO.succeed(a)
    }

  private def encode[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)

  private def merge(
    d1: DynamicValue,
    d2: DynamicValue,
    a1: SchemaAst,
    a2: SchemaAst,
  ): Either[String, DynamicValue] = {
    val s1 = a1.toSchema.asInstanceOf[Schema[Any]]
    val s2 = a2.toSchema.asInstanceOf[Schema[Any]]
    for {
      v1 <- d1.toTypedValue(s1)
      v2 <- d2.toTypedValue(s2)
    } yield (s1 <*> s2).toDynamic((v1, v2))
  }

  final case class Scope[S, K, V](ref: Ref[Map[(S, K), V]]) {
    def get(scope: S, key: K): UIO[Option[V]] = ref.get.map(_.get(scope, key))

    def set(scope: S, key: K, value: V): UIO[Unit] = ref.update { map => map + (((scope, key), value)) }

    def deleteScope(scope: S): UIO[Unit] = ref.update { map => map.filter { case s -> _ -> _ => s != scope } }
  }

  object Scope {
    def make[S, K, V]: UIO[Scope[S, K, V]] = Ref.make(Map.empty[(S, K), V]).map(Scope(_))
  }
}
