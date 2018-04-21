package org.ucombinator.jade.jvm.classfile.descriptor

import org.ucombinator.jade.jvm.classfile.TypeCommons.JavaIdentifier
import org.ucombinator.jade.method.Val

import scala.annotation.tailrec

trait Descriptor

object Descriptor {
  /** Field Descriptors */
  sealed abstract class FieldDescriptor extends Descriptor

  abstract class FieldType extends FieldDescriptor with ReturnDescriptor

  trait ArrayElementType extends Val  // Tag trait
  trait ReferenceType extends Val

  // TODO: ClassName -- $4.2.1
  sealed case class ObjectType(packageSpecifier: List[JavaIdentifier], className: JavaIdentifier)
    extends FieldType with ArrayElementType with ReferenceType

  sealed case class ArrayType(componentType: ComponentType) extends FieldType with ReferenceType {
    val typ: ArrayElementType = {
      @tailrec
      def unwrap(ft: FieldType): ArrayElementType =
        ft match {
          case ArrayType(t)        => unwrap(t)
          case t: ArrayElementType => t
        }

      unwrap(this)
    }
  }

  type ComponentType = FieldType

  /** Method Descriptors */
  case class MethodDescriptor(parameterDescriptors: List[ParameterDescriptor],
                              returnDescriptor: ReturnDescriptor)
    extends Descriptor

  type ParameterDescriptor = FieldType

  trait ReturnDescriptor extends Descriptor
  case object VoidDescriptor extends ReturnDescriptor
}

