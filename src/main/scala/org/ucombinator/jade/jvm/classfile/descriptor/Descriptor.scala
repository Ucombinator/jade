package org.ucombinator.jade.jvm.classfile.descriptor

import org.ucombinator.jade.jvm.classfile.TypeCommons.JavaIdentifier

import scala.annotation.tailrec


trait Descriptor

object Descriptor {
  /** Field Descriptors */
  sealed abstract class FieldDescriptor extends Descriptor

  abstract class FieldType extends FieldDescriptor with ReturnDescriptor

  abstract class ReferenceType extends FieldType
  abstract class PrimitiveType extends FieldType

  // TODO: ClassName -- $4.2.1
  sealed case class ObjectType(packageSpecifier: List[JavaIdentifier], className: JavaIdentifier)
    extends ReferenceType

  sealed case class ArrayType(componentType: ComponentType) extends ReferenceType {
    val typ: FieldDescriptor = {
      @tailrec
      def unwrap(ft: FieldType): FieldDescriptor =
        ft match {
          case ArrayType(t)        => unwrap(t)
          case t                   => t
        }

      unwrap(this)
    }
  }

  type ComponentType = FieldType

  /** Method Descriptors */
  sealed case class MethodDescriptor(parameterDescriptors: List[ParameterDescriptor],
                                     returnDescriptor: ReturnDescriptor)
    extends Descriptor

  type ParameterDescriptor = FieldType

  sealed trait ReturnDescriptor extends Descriptor
  case object VoidDescriptor extends ReturnDescriptor
}

