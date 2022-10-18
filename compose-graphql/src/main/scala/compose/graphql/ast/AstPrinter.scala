package compose.graphql.ast

object AstPrinter {
  import Ast._
  def render(self: Ast): String = self match {
    case Ast.Document(definitions) =>
      s"\n${definitions.sortBy(_.name).map(_.encode).mkString("\n")}\n"

    case definitions: Definitions => definitions match {
        case Definitions.ObjectType(name, fields) =>
          s"type $name {\n  ${fields.sortBy(_.name).map(_.encode).mkString("\n  ")}\n}"

        case Definitions.InputValue(name, fieldType) => s"$name: ${fieldType.encode}"

        case Definitions.Field(name, arguments, fieldType) =>
          val args =
            if (arguments.isEmpty) ""
            else s"(${arguments.sortBy(_.name).map(_.encode).mkString(", ")})"
          val tpe  = fieldType.encode
          s"$name$args: $tpe"
      }

    case fieldType: Ast.Type => fieldType match {
        case Type.Named(name)  => name
        case Type.NotNull(tpe) => s"${tpe.encode}!"
        case Type.List(tpe)    => s"[${tpe.encode}]"
      }
  }
}
