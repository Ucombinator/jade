package org.ucombinator.jade.typeinfo

object TypeInfo {

  final case class Id(id: String) {
    private def isJavaIdentifier(id: String): Boolean = {
      require(id != null, "mustn't be `null`")
      java.lang.Character.isJavaIdentifierStart(id.head) &&
        id.tail.forall(java.lang.Character.isJavaIdentifierPart)
    }

    require(isJavaIdentifier(id), "An illegal Java identifier!")
  }

  sealed abstract class Bound
  case object Extends extends Bound
  case object Super extends Bound

  sealed trait TReturnable
  sealed trait TThrowable

  /** Type Signature */
  sealed abstract class TSignature

  sealed abstract class TypeSignature extends TSignature with TReturnable
  case object B extends TypeSignature  // `byte`   - signed byte
  case object C extends TypeSignature  // `char`   - Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16
  case object D extends TypeSignature  // `double` - double-precision floating-point value
  case object F extends TypeSignature  // `float`  - single-precision floating-point value
  case object I extends TypeSignature  // `int`    - integer
  case object J extends TypeSignature  // `long`   - long integer
  case object S extends TypeSignature  // `short`   - signed short
  case object Z extends TypeSignature  // `boolean` - true or false

  sealed abstract class FieldTypeSignature extends TypeSignature
  final case class L(pkg: List[Id], typeArgs: (Id, List[TypeArg])*) extends FieldTypeSignature with TThrowable // TODO:
  type ClassTypeSignature = L
  final case class  `[`(arr: TypeSignature) extends FieldTypeSignature
  type TypeArgs = List[TypeArg]
  final case class TypeArg(bound: Option[Bound], fts: FieldTypeSignature)
  final case class TypeVar(name: Id) extends TThrowable

  /** MethodTypeSignature */
  case object V extends TSignature with TReturnable

  final case class MethodTypeSignature(TypeParameters: List[TypeParam],
                                       parameterTypeSignatures: List[TypeSignature],
                                       returnType: TReturnable,
                                       exceptions: List[CheckedException]) extends TSignature

  final case class CheckedException(ex: TThrowable)
  type TypeParams = List[TypeParam]
  final case class TypeParam(name: Id, bounds: List[FieldTypeSignature]) extends TSignature


  final case class ClassSignature(typeParameters:TypeParams,
                                  selfClassSignature: L,
                                  superClassSignature: L,
                                  interfaceSignatures: List[L]) extends TSignature

  def stringToSignature(typeSignature: String): TSignature = typeSignature match {
    case "B" | "byte"    => B
    case "C" | "char"    => C
    case "D" | "double"  => D
    case "F" | "float"   => F
    case "I" | "int"     => I
    case "J" | "long"    => J
    case "S" | "short"   => S
    case "Z" | "boolean" => Z
    case typ if typ.startsWith("L") => L(Nil, null) // TODO: placeholder
    case typ if typ.startsWith("[") => `[`(null)    // TODO: placeholder
    case "V" | "void"    => V
    ???
  }
}
