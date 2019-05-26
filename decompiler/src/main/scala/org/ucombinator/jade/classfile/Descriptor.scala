package org.ucombinator.jade.classfile

import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, Type}
import com.github.javaparser.ast.expr.Name

// Parsing signatures as defined in the JVM Specification section 4.3
object Descriptor {
  def typeDescriptor(string: String): Type = {
    Signature.typeSignature(string)
  }
  def methodDescriptor(string: String): (Array[Type], Type) = {
    val s = Signature.methodSignature(string)
    assert(s._1.isEmpty)
    assert(s._4.isEmpty)
    (s._2, s._3)
  }
  def className(string: String): Name = {
    string.split('/').foldLeft(null: Name){ (qualifier, identifier) => new Name(qualifier, identifier) }
  }
  def nameToType(string: String): ClassOrInterfaceType = {
    nameToType(className(string))
  }
  def nameToType(name: Name): ClassOrInterfaceType = {
    if (name == null) { null }
    else { new ClassOrInterfaceType(nameToType(name.getQualifier.orElse(null)), name.getIdentifier) }
  }
}
