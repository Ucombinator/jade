package org.ucombinator.jade.classfile

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, Type}
import com.github.javaparser.ast.expr.{Expression, FieldAccessExpr, Name, NameExpr, SimpleName}

// NOTE: The structure of this class follows that of the Java Virtual Machine Specification section 4.3 "Descriptors"
object Descriptor {
  def fieldDescriptor(string: String): Type = {
    Signature.typeSignature(string)
  }
  def methodDescriptor(string: String): (Array[Type], Type) = {
    val s = Signature.methodSignature(string)
    assert(s._1.isEmpty)
    assert(s._4.isEmpty)
    (s._2, s._3)
  }
  // TODO: move to ClassName?
  def className(string: String): Name = {
    string.split('/').foldLeft(null: Name){ (qualifier, identifier) => new Name(qualifier, identifier) }
  }
  def classNameExpr(string: String): Expression = {
    string.split('/').foldLeft(null: Expression){
      case (null, identifier) => new NameExpr(new SimpleName(identifier))
      case (qualifier, identifier) => new FieldAccessExpr(qualifier, /*TODO*/new NodeList(), new SimpleName(identifier)) }
  }
  def classNameType(string: String): ClassOrInterfaceType = {
    classNameType(className(string))
  }
  def classNameType(name: Name): ClassOrInterfaceType = {
    if (name == null) { null }
    else { new ClassOrInterfaceType(classNameType(name.getQualifier.orElse(null)), name.getIdentifier) }
  }
}
