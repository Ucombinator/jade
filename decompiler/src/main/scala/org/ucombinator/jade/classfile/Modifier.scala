// Do not edit this file by hand.  It is generated by `jade generate-modifier-code`.
package org.ucombinator.jade.classfile

sealed trait Modifier {
  def value: Int
  def keyword: Option[com.github.javaparser.ast.Modifier.Keyword]
  def valueAsString: String = f"0x$value%04x"
  def modifier: Option[com.github.javaparser.ast.Modifier] = keyword.map(new com.github.javaparser.ast.Modifier(_))
}

sealed trait ClassModifier extends Modifier
sealed trait FieldModifier extends Modifier
sealed trait MethodModifier extends Modifier
sealed trait NestedClassModifier extends Modifier
sealed trait ParameterModifier extends Modifier
sealed trait ModuleModifier extends Modifier
sealed trait RequiresModifier extends Modifier
sealed trait ExportsModifier extends Modifier
sealed trait OpensModifier extends Modifier

sealed class ModifierImpl(
  override val value: Int,
  override val keyword: Option[com.github.javaparser.ast.Modifier.Keyword])
  extends Modifier

object Modifier {
  def modifiersToNodeList(modifiers: List[Modifier]):
    com.github.javaparser.ast.NodeList[com.github.javaparser.ast.Modifier] = {
    import scala.collection.JavaConverters._
    new com.github.javaparser.ast.NodeList(modifiers.flatMap(_.modifier).asJava)
  }

  private def fromInt[T](mapping: List[(Int, T)]): Int => List[T] = {
    int: Int => {
      val maskedInt = int & 0xFFFF // Ignore ASM specific flags, which occur above bit 16
      val result = mapping.filter(pair => (pair._1 & maskedInt) != 0)
      val intResult = result.map(_._1).fold(0)(_ | _)
      assert(maskedInt == intResult, f"modifier parsing error: want 0x$int%x, got 0x$intResult%x")
      result.map(_._2)
    }
  }

  case object ACC_PUBLIC
    extends ModifierImpl(0x0001, Some(com.github.javaparser.ast.Modifier.Keyword.PUBLIC))
    with ClassModifier with FieldModifier with MethodModifier with NestedClassModifier
  case object ACC_PRIVATE
    extends ModifierImpl(0x0002, Some(com.github.javaparser.ast.Modifier.Keyword.PRIVATE))
    with FieldModifier with MethodModifier with NestedClassModifier
  case object ACC_PROTECTED
    extends ModifierImpl(0x0004, Some(com.github.javaparser.ast.Modifier.Keyword.PROTECTED))
    with FieldModifier with MethodModifier with NestedClassModifier
  case object ACC_STATIC
    extends ModifierImpl(0x0008, Some(com.github.javaparser.ast.Modifier.Keyword.STATIC))
    with FieldModifier with MethodModifier with NestedClassModifier
  case object ACC_FINAL
    extends ModifierImpl(0x0010, Some(com.github.javaparser.ast.Modifier.Keyword.FINAL))
    with ClassModifier with FieldModifier with MethodModifier with NestedClassModifier with ParameterModifier
  case object ACC_OPEN
    extends ModifierImpl(0x0020, None)
    with ModuleModifier
  case object ACC_SUPER
    extends ModifierImpl(0x0020, None)
    with ClassModifier
  case object ACC_SYNCHRONIZED
    extends ModifierImpl(0x0020, Some(com.github.javaparser.ast.Modifier.Keyword.SYNCHRONIZED))
    with MethodModifier
  case object ACC_TRANSITIVE
    extends ModifierImpl(0x0020, Some(com.github.javaparser.ast.Modifier.Keyword.TRANSITIVE))
    with RequiresModifier
  case object ACC_BRIDGE
    extends ModifierImpl(0x0040, None)
    with MethodModifier
  case object ACC_STATIC_PHASE
    extends ModifierImpl(0x0040, None)
    with RequiresModifier
  case object ACC_VOLATILE
    extends ModifierImpl(0x0040, Some(com.github.javaparser.ast.Modifier.Keyword.VOLATILE))
    with FieldModifier
  case object ACC_TRANSIENT
    extends ModifierImpl(0x0080, Some(com.github.javaparser.ast.Modifier.Keyword.TRANSIENT))
    with FieldModifier
  case object ACC_VARARGS
    extends ModifierImpl(0x0080, None)
    with MethodModifier
  case object ACC_NATIVE
    extends ModifierImpl(0x0100, Some(com.github.javaparser.ast.Modifier.Keyword.NATIVE))
    with MethodModifier
  case object ACC_INTERFACE
    extends ModifierImpl(0x0200, None)
    with ClassModifier with NestedClassModifier
  case object ACC_ABSTRACT
    extends ModifierImpl(0x0400, Some(com.github.javaparser.ast.Modifier.Keyword.ABSTRACT))
    with ClassModifier with MethodModifier with NestedClassModifier
  case object ACC_STRICT
    extends ModifierImpl(0x0800, Some(com.github.javaparser.ast.Modifier.Keyword.STRICTFP))
    with MethodModifier
  case object ACC_SYNTHETIC
    extends ModifierImpl(0x1000, None)
    with ClassModifier with FieldModifier with MethodModifier with NestedClassModifier with ParameterModifier with ModuleModifier with RequiresModifier with ExportsModifier with OpensModifier
  case object ACC_ANNOTATION
    extends ModifierImpl(0x2000, None)
    with ClassModifier with NestedClassModifier
  case object ACC_ENUM
    extends ModifierImpl(0x4000, None)
    with ClassModifier with FieldModifier with NestedClassModifier
  case object ACC_MANDATED
    extends ModifierImpl(0x8000, None)
    with ParameterModifier with ModuleModifier with RequiresModifier with ExportsModifier with OpensModifier
  case object ACC_MODULE
    extends ModifierImpl(0x8000, None)
    with ClassModifier

  private val ClassMapping = List[(Int, ClassModifier)](
    (/*0x0001*/ ACC_PUBLIC.value, ACC_PUBLIC), // Declared public; may be accessed from outside its package.
    (/*0x0010*/ ACC_FINAL.value, ACC_FINAL), // Declared final; no subclasses allowed.
    (/*0x0020*/ ACC_SUPER.value, ACC_SUPER), // Treat superclass methods specially when invoked by the invokespecial instruction.
    (/*0x0200*/ ACC_INTERFACE.value, ACC_INTERFACE), // Is an interface, not a class.
    (/*0x0400*/ ACC_ABSTRACT.value, ACC_ABSTRACT), // Declared abstract; must not be instantiated.
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Declared synthetic; not present in the source code.
    (/*0x2000*/ ACC_ANNOTATION.value, ACC_ANNOTATION), // Declared as an annotation type.
    (/*0x4000*/ ACC_ENUM.value, ACC_ENUM), // Declared as an enum type.
    (/*0x8000*/ ACC_MODULE.value, ACC_MODULE), // Is a module, not a class or interface.
  )
  private val FieldMapping = List[(Int, FieldModifier)](
    (/*0x0001*/ ACC_PUBLIC.value, ACC_PUBLIC), // Declared public; may be accessed from outside its package.
    (/*0x0002*/ ACC_PRIVATE.value, ACC_PRIVATE), // Declared private; accessible only within the defining class and other classes belonging to the same nest (§5.4.4).
    (/*0x0004*/ ACC_PROTECTED.value, ACC_PROTECTED), // Declared protected; may be accessed within subclasses.
    (/*0x0008*/ ACC_STATIC.value, ACC_STATIC), // Declared static.
    (/*0x0010*/ ACC_FINAL.value, ACC_FINAL), // Declared final; never directly assigned to after object construction (JLS §17.5).
    (/*0x0040*/ ACC_VOLATILE.value, ACC_VOLATILE), // Declared volatile; cannot be cached.
    (/*0x0080*/ ACC_TRANSIENT.value, ACC_TRANSIENT), // Declared transient; not written or read by a persistent object manager.
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Declared synthetic; not present in the source code.
    (/*0x4000*/ ACC_ENUM.value, ACC_ENUM), // Declared as an element of an enum.
  )
  private val MethodMapping = List[(Int, MethodModifier)](
    (/*0x0001*/ ACC_PUBLIC.value, ACC_PUBLIC), // Declared public; may be accessed from outside its package.
    (/*0x0002*/ ACC_PRIVATE.value, ACC_PRIVATE), // Declared private; accessible only within the defining class and other classes belonging to the same nest (§5.4.4).
    (/*0x0004*/ ACC_PROTECTED.value, ACC_PROTECTED), // Declared protected; may be accessed within subclasses.
    (/*0x0008*/ ACC_STATIC.value, ACC_STATIC), // Declared static.
    (/*0x0010*/ ACC_FINAL.value, ACC_FINAL), // Declared final; must not be overridden (§5.4.5).
    (/*0x0020*/ ACC_SYNCHRONIZED.value, ACC_SYNCHRONIZED), // Declared synchronized; invocation is wrapped by a monitor use.
    (/*0x0040*/ ACC_BRIDGE.value, ACC_BRIDGE), // A bridge method, generated by the compiler.
    (/*0x0080*/ ACC_VARARGS.value, ACC_VARARGS), // Declared with variable number of arguments.
    (/*0x0100*/ ACC_NATIVE.value, ACC_NATIVE), // Declared native; implemented in a language other than the Java programming language.
    (/*0x0400*/ ACC_ABSTRACT.value, ACC_ABSTRACT), // Declared abstract; no implementation is provided.
    (/*0x0800*/ ACC_STRICT.value, ACC_STRICT), // Declared strictfp; floating-point mode is FP-strict.
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Declared synthetic; not present in the source code.
  )
  private val NestedClassMapping = List[(Int, NestedClassModifier)](
    (/*0x0001*/ ACC_PUBLIC.value, ACC_PUBLIC), // Marked or implicitly public in source.
    (/*0x0002*/ ACC_PRIVATE.value, ACC_PRIVATE), // Marked private in source.
    (/*0x0004*/ ACC_PROTECTED.value, ACC_PROTECTED), // Marked protected in source.
    (/*0x0008*/ ACC_STATIC.value, ACC_STATIC), // Marked or implicitly static in source.
    (/*0x0010*/ ACC_FINAL.value, ACC_FINAL), // Marked or implicitly final in source.
    (/*0x0200*/ ACC_INTERFACE.value, ACC_INTERFACE), // Was an interface in source.
    (/*0x0400*/ ACC_ABSTRACT.value, ACC_ABSTRACT), // Marked or implicitly abstract in source.
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Declared synthetic; not present in the source code.
    (/*0x2000*/ ACC_ANNOTATION.value, ACC_ANNOTATION), // Declared as an annotation type.
    (/*0x4000*/ ACC_ENUM.value, ACC_ENUM), // Declared as an enum type.
  )
  private val ParameterMapping = List[(Int, ParameterModifier)](
    (/*0x0010*/ ACC_FINAL.value, ACC_FINAL), // Indicates that the formal parameter was declared final.
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Indicates that the formal parameter was not explicitly or implicitly declared in source code, according to the specification of the language in which the source code was written (JLS §13.1). (The formal parameter is an implementation artifact of the compiler which produced this class file.)
    (/*0x8000*/ ACC_MANDATED.value, ACC_MANDATED), // Indicates that the formal parameter was implicitly declared in source code, according to the specification of the language in which the source code was written (JLS §13.1). (The formal parameter is mandated by a language specification, so all compilers for the language must emit it.)
  )
  private val ModuleMapping = List[(Int, ModuleModifier)](
    (/*0x0020*/ ACC_OPEN.value, ACC_OPEN), // Indicates that this module is open.
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Indicates that this module was not explicitly or implicitly declared.
    (/*0x8000*/ ACC_MANDATED.value, ACC_MANDATED), // Indicates that this module was implicitly declared.
  )
  private val RequiresMapping = List[(Int, RequiresModifier)](
    (/*0x0020*/ ACC_TRANSITIVE.value, ACC_TRANSITIVE), // Indicates that any module which depends on the current module, implicitly declares a dependence on the module indicated by this entry.
    (/*0x0040*/ ACC_STATIC_PHASE.value, ACC_STATIC_PHASE), // Indicates that this dependence is mandatory in the static phase, i.e., at compile time, but is optional in the dynamic phase, i.e., at run time.
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Indicates that this dependence was not explicitly or implicitly declared in the source of the module declaration.
    (/*0x8000*/ ACC_MANDATED.value, ACC_MANDATED), // Indicates that this dependence was implicitly declared in the source of the module declaration.
  )
  private val ExportsMapping = List[(Int, ExportsModifier)](
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Indicates that this export was not explicitly or implicitly declared in the source of the module declaration.
    (/*0x8000*/ ACC_MANDATED.value, ACC_MANDATED), // Indicates that this export was implicitly declared in the source of the module declaration.
  )
  private val OpensMapping = List[(Int, OpensModifier)](
    (/*0x1000*/ ACC_SYNTHETIC.value, ACC_SYNTHETIC), // Indicates that this opening was not explicitly or implicitly declared in the source of the module declaration.
    (/*0x8000*/ ACC_MANDATED.value, ACC_MANDATED), // Indicates that this opening was implicitly declared in the source of the module declaration.
  )

  val intToClass: Int => List[ClassModifier] = fromInt(ClassMapping)
  val intToField: Int => List[FieldModifier] = fromInt(FieldMapping)
  val intToMethod: Int => List[MethodModifier] = fromInt(MethodMapping)
  val intToNestedClass: Int => List[NestedClassModifier] = fromInt(NestedClassMapping)
  val intToParameter: Int => List[ParameterModifier] = fromInt(ParameterMapping)
  val intToModule: Int => List[ModuleModifier] = fromInt(ModuleMapping)
  val intToRequires: Int => List[RequiresModifier] = fromInt(RequiresMapping)
  val intToExports: Int => List[ExportsModifier] = fromInt(ExportsMapping)
  val intToOpens: Int => List[OpensModifier] = fromInt(OpensMapping)
}
