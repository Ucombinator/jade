package org.ucombinator.jade.classfile

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, ReferenceType, Type, VoidType, WildcardType, TypeParameter => JPTypeParamter}
import com.github.javaparser.ast.expr.{AnnotationExpr, SimpleName}

import scala.collection.JavaConverters._
import scala.util.parsing.combinator.RegexParsers
import sun.reflect.generics.parser.SignatureParser
import sun.reflect.generics.tree.{ArrayTypeSignature, BaseType, BooleanSignature, BottomSignature, ByteSignature, CharSignature, ClassSignature, ClassTypeSignature, DoubleSignature, FieldTypeSignature, FloatSignature, FormalTypeParameter, IntSignature, LongSignature, MethodTypeSignature, ReturnType, ShortSignature, SimpleClassTypeSignature, TypeArgument, TypeSignature, TypeVariableSignature, VoidDescriptor, Wildcard}

// Grammar defined in JVMS 4.7.9.1
object Signature extends RegexParsers {
  def classSignature2(string: String): (Array[JPTypeParamter], ClassOrInterfaceType, Array[ClassOrInterfaceType]) = {
    val oldS = Signature.classSignature(string)
    val s = parse3(SignatureParser.make().parseClassSig(string))
    if ((s._1.toList, s._2, s._3.toList).toString() != oldS.toString()) {
      assert(false)
    }
    s
  }
  def methodSignature2(string: String): (Array[JPTypeParamter], Array[Type], Type, Array[ReferenceType]) = {
    val oldS = Signature.methodSignature(string)
    val s = parse3(SignatureParser.make().parseMethodSig(string))
    if ((s._1.toList, s._2.toList, s._3, s._4.toList).toString() != oldS.toString()) {
      assert(false)
    }
    s
  }
  def typeSignature2(string: String): Type = {
    val s = parse3(SignatureParser.make().parseTypeSig(string))
    val oldS = javaTypeSignature(string)
    if (s.toString != oldS.toString) {
      assert(false)
    }
    s
  }

  def parse3(methodTypeSignature: MethodTypeSignature): (Array[JPTypeParamter], Array[Type], Type, Array[ReferenceType]) = {
    (methodTypeSignature.getFormalTypeParameters.map(parse3),
    methodTypeSignature.getParameterTypes.map(parse3),
    parse3(methodTypeSignature.getReturnType),
    methodTypeSignature.getExceptionTypes.map(parse3).map(_.asInstanceOf[ReferenceType]))
  }
  def parse3(returnType: ReturnType): Type = {
    returnType match {
      case returnType: VoidDescriptor => new VoidType
      case returnType: TypeSignature => parse3(returnType)
    }
  }
  def parse3(classSig: ClassSignature): (Array[JPTypeParamter], ClassOrInterfaceType, Array[ClassOrInterfaceType]) = {
    (classSig.getFormalTypeParameters.map(parse3),
    parse3(classSig.getSuperclass),
    classSig.getSuperInterfaces.map(parse3))
  }
  def parse3(formalTypeParameter: FormalTypeParameter): JPTypeParamter = {
    new JPTypeParamter(
      formalTypeParameter.getName,
      new NodeList(formalTypeParameter.getBounds.map(parse3).map(_.asInstanceOf[ClassOrInterfaceType]):_*))
  }
  def parse3(fieldTypeSignature: FieldTypeSignature): Type = {
    fieldTypeSignature match {
      case fieldTypeSignature: ArrayTypeSignature => parse3(fieldTypeSignature)
      case fieldTypeSignature: BottomSignature => parse3(fieldTypeSignature)
      case fieldTypeSignature: ClassTypeSignature => parse3(fieldTypeSignature)
      case fieldTypeSignature: SimpleClassTypeSignature => parse3(fieldTypeSignature)
      case fieldTypeSignature: TypeVariableSignature => parse3(fieldTypeSignature)
    }
  }
  def parse3(bottomSignature: BottomSignature): Type = {
    ???
  }
  def parse3(typeVariableSignature: TypeVariableSignature): Type = {
    new JPTypeParamter(typeVariableSignature.getIdentifier)
  }
  def parse3(arrayTypeSignature: ArrayTypeSignature): Type = {
    new ArrayType(parse3(arrayTypeSignature.getComponentType))
  }
  def parse3(classTypeSignature: ClassTypeSignature): ClassOrInterfaceType = {
    classTypeSignature.getPath.asScala
      .foldLeft(null: ClassOrInterfaceType)
      { (scope, simpleClassTypeSignature) => parse3(scope, simpleClassTypeSignature) }
  }
  def parse3(initialScope: ClassOrInterfaceType = null, simpleClassTypeSignature: SimpleClassTypeSignature): ClassOrInterfaceType = {
    val name :: names = simpleClassTypeSignature.getName.split('.').toList.reverse
    val scope = names.foldRight(initialScope){ (name, scope) => new ClassOrInterfaceType(scope, name) }
    // TODO: ignored: simpleClassTypeSignature.getDollar
    new ClassOrInterfaceType(
      scope,
      new SimpleName(name),
      if (simpleClassTypeSignature.getTypeArguments.isEmpty) { null }
      else { new NodeList(simpleClassTypeSignature.getTypeArguments.map(parse3):_*) })
  }
  def parse3(typeArgument: TypeArgument): Type = {
    typeArgument match {
      case typeArgument: FieldTypeSignature => parse3(typeArgument)
      case typeArgument: Wildcard => parse3(typeArgument)
    }
  }
  def parse3(wildcard: Wildcard): WildcardType = {
    (wildcard.getLowerBounds, wildcard.getUpperBounds) match {
      case (Array(), Array(o: SimpleClassTypeSignature)) if o.getName == "java.lang.Object" =>
        // *
        new WildcardType()
      case (Array(), Array(t)) =>
        // +
        // TODO: use Type.asReferenceType
        new WildcardType(parse3(t).asInstanceOf[ReferenceType])
      case (Array(t), Array(o: SimpleClassTypeSignature)) if o.getName == "java.lang.Object" =>
        // -
        new WildcardType(null, parse3(t).asInstanceOf[ReferenceType], new NodeList[AnnotationExpr]())
    }
  }
  def parse3(typeSignature: TypeSignature): Type = {
    typeSignature match {
      case typeSignature: FieldTypeSignature => parse3(typeSignature)
      case typeSignature: BaseType => parse3(typeSignature)
    }
  }
  def parse3(baseType: BaseType): Type = {
    baseType match {
      case baseType: BooleanSignature => PrimitiveType.booleanType()
      case baseType: ByteSignature => PrimitiveType.byteType()
      case baseType: CharSignature => PrimitiveType.charType()
      case baseType: DoubleSignature => PrimitiveType.doubleType()
      case baseType: FloatSignature => PrimitiveType.floatType()
      case baseType: IntSignature => PrimitiveType.intType()
      case baseType: LongSignature => PrimitiveType.longType()
      case baseType: ShortSignature => PrimitiveType.shortType()
      case baseType: FieldTypeSignature => ??? // TODO: is this possible?
    }
  }


  private def parse[T](p: Parser[T], name: String): String => T = {
    string: String =>
      parseAll(p, string) match {
        case Error(msg, _) => throw new Exception(f"parse error '$msg' in $name '$string'")
        case Failure(msg, _) => throw new Exception(f"parse failure '$msg' in $name '$string'")
        case Success(typ, i) =>
          if (!i.atEnd) { throw new Exception(f"parse incomplete at position ${i.pos} in $name '$string'") }
          else { typ }
      }
  }
  val javaTypeSignature: String => Type = { parse(JavaTypeSignature, "Java type signature") }
  val classSignature: String => (List[JPTypeParamter], ClassOrInterfaceType, List[ClassOrInterfaceType]) = {
    parse(ClassSignature, "class signature") }
  val methodSignature: String => (List[JPTypeParamter], List[Type], Type, List[ReferenceType]) = {
    parse(MethodSignature, "method signature") }
  val fieldSignature: String => Type = { parse(FieldSignature, "field signature") }

  // Used by `javaTypeSignature`
  private lazy val JavaTypeSignature: Parser[Type] =
    ReferenceTypeSignature |
    BaseType

  private lazy val BaseType: Parser[Type] =
    'B' ^^^ PrimitiveType.byteType() |
    'C' ^^^ PrimitiveType.charType() |
    'D' ^^^ PrimitiveType.doubleType() |
    'F' ^^^ PrimitiveType.floatType() |
    'I' ^^^ PrimitiveType.intType() |
    'J' ^^^ PrimitiveType.longType() |
    'S' ^^^ PrimitiveType.shortType() |
    'Z' ^^^ PrimitiveType.booleanType()

  private lazy val Identifier: Parser[String] = "[^.;\\[/<>:]*".r

  private lazy val ReferenceTypeSignature: Parser[ReferenceType] =
    ClassTypeSignature |
    TypeVariableSignature |
    ArrayTypeSignature

  private lazy val ClassTypeSignature: Parser[ClassOrInterfaceType] =
    'L' ~> PackageSpecifier.? ~ SimpleClassTypeSignature ~ ClassTypeSignatureSuffix.* <~ ';' ^^ {
      case packageSpecifier ~ simpleClassTypeSignature ~ classTypeSignatureSuffix =>
        val type1 = packageSpecifier.toList.flatten.foldLeft(null: ClassOrInterfaceType){ (scope, name) => new ClassOrInterfaceType(scope, name) }
        val type2 = new ClassOrInterfaceType(type1, simpleClassTypeSignature.getName, simpleClassTypeSignature.getTypeArguments.orElse(null))
        val type3 = classTypeSignatureSuffix.foldLeft(type2){ (scope, suffix) => new ClassOrInterfaceType(scope, suffix.getName, suffix.getTypeArguments.orElse(null)) }
        type3 }

  private lazy val PackageSpecifier: Parser[List[String]] =
    Identifier ~ '/' ~ PackageSpecifier.? ^^ { // Typo in spec has {} when it should be []
      case identifier ~ _ ~ None => List(identifier)
      case identifier ~ _ ~ Some(packageSpecifier) => identifier :: packageSpecifier }

  private lazy val SimpleClassTypeSignature: Parser[ClassOrInterfaceType] =
    Identifier ~ TypeArguments.? ^^ {
      case identifier ~ None => new ClassOrInterfaceType(null, identifier)
      case identifier ~ Some(typeArguments) => new ClassOrInterfaceType(null, new SimpleName(identifier), new NodeList[Type](typeArguments.asJava)) }

  private lazy val TypeArguments: Parser[List[Type]] =
    '<' ~> TypeArgument ~ TypeArgument.* <~ '>' ^^ { case typeArgument ~ typeArguments => typeArgument :: typeArguments}

  private lazy val TypeArgument: Parser[Type] =
    WildcardIndicator.? ~ ReferenceTypeSignature ^^ {
      case None ~ referenceTypeSignature => referenceTypeSignature
      case Some(true) ~ referenceTypeSignature => new WildcardType(referenceTypeSignature) // TODO: these might be backwards
      case Some(false) ~ referenceTypeSignature => new WildcardType(null: ReferenceType, referenceTypeSignature, new NodeList[AnnotationExpr]()) } |
    '*' ^^^ new WildcardType()

  private lazy val WildcardIndicator: Parser[Boolean] =
    '+' ^^^ true |
    '-' ^^^ false

  private lazy val ClassTypeSignatureSuffix: Parser[ClassOrInterfaceType] =
    '.' ~> SimpleClassTypeSignature

  private lazy val TypeVariableSignature: Parser[ReferenceType] =
    'T' ~> Identifier <~ ';' ^^ { new JPTypeParamter(_) }

  private lazy val ArrayTypeSignature: Parser[ReferenceType] =
    '[' ~> JavaTypeSignature ^^ { new ArrayType(_) }

  // Used by `classSignature`
  private lazy val ClassSignature: Parser[(List[JPTypeParamter], ClassOrInterfaceType, List[ClassOrInterfaceType])] =
    TypeParameters.? ~ SuperclassSignature ~ SuperinterfaceSignature.* ^^ {
      case typeParameters ~ superclassSignature ~ superinterfaceSignature =>
        (typeParameters.getOrElse(List()), superclassSignature, superinterfaceSignature) }

  private lazy val TypeParameters: Parser[List[JPTypeParamter]] =
    '<' ~> TypeParameter ~ TypeParameter.* <~ '>' ^^ {
      case typeParameter ~ typeParameters => typeParameter :: typeParameters }

  private lazy val TypeParameter: Parser[JPTypeParamter] =
    Identifier ~ ClassBound ~ InterfaceBound.* ^^ {
      case name ~ classBound ~ interfaceBound =>
        new JPTypeParamter(name, new NodeList((classBound :: interfaceBound).flatten.asJava)) }

  private lazy val ClassBound: Parser[Option[ClassOrInterfaceType]] =
    ':' ~> ReferenceTypeSignature.? ^^ { _.asInstanceOf[Option[ClassOrInterfaceType]] }

  private lazy val InterfaceBound: Parser[Option[ClassOrInterfaceType]] =
    ':' ~> ReferenceTypeSignature.? ^^ { _.asInstanceOf[Option[ClassOrInterfaceType]] }

  private lazy val SuperclassSignature: Parser[ClassOrInterfaceType] =
    ClassTypeSignature

  private lazy val SuperinterfaceSignature: Parser[ClassOrInterfaceType] =
    ClassTypeSignature

  // Used by `methodSignature`
  private lazy val MethodSignature: Parser[(List[JPTypeParamter], List[Type], Type, List[ReferenceType])] =
    TypeParameters.? ~ ('(' ~> JavaTypeSignature.* <~ ')') ~ Result ~ ThrowsSignature.* ^^ {
      case typeParameters ~ javaTypeSignature ~ result ~ throwsSignature =>
        (typeParameters.getOrElse(List()), javaTypeSignature, result, throwsSignature) }

  private lazy val Result: Parser[Type] =
    JavaTypeSignature |
    VoidDescriptor

  private lazy val ThrowsSignature: Parser[ReferenceType] =
    '^' ~> ClassTypeSignature |
    '^' ~> TypeVariableSignature

  private lazy val VoidDescriptor: Parser[Type] =
    'V' ^^^ new VoidType

  // Used by `fieldSignature`
  private lazy val FieldSignature: Parser[Type] =
    ReferenceTypeSignature
}
