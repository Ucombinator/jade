package org.ucombinator.jade.typeinfo

import scala.util.matching.Regex
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.util.TraceSignatureVisitor
import org.objectweb.asm.signature.SignatureReader
import org.ucombinator.jade.AccessFlag

import scala.collection.JavaConverters._

object TypeParser {
  import TypeInfo._

  private val classSignaturePattern = raw"^(<[^>]+>)".r


  // TODO: extractClassSignature --> ClassTypeSignature.apply
  def extractClassSignature(signature: String): ClassTypeSignature = {
    // "Ljava/util/AbstractMap<TK;TV;>;"
    // TODO: CLASS/INTERFACE signature
    //  "<K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/util/AbstractMap<TK;TV;>;Ljava/util/Map<TK;TV;>;Ljava/lang/Cloneable;Ljava/io/Serializable;"

    ???
  }


  // TODO: extractMethodSignature --> MethodTypeSignature.apply
  def extractMethodSignature(method: MethodNode):  MethodTypeSignature = {
    val tsv = new TraceSignatureVisitor(method.access)
    val sr = Option(method.signature) match {
      case None => new SignatureReader(method.desc)
      case _    => new SignatureReader(method.signature)
    }

    sr.accept(tsv)

    val exceptions = Option(method.signature) match {
      case None =>
        Option(method.exceptions) match {
          case None      => ""
          case Some(exs) => " throws " + exs.asScala.map(_.replaceAll("/", ".")).mkString(" ")
        }
      case Some(s) => " throws " + s // TODO: transform
    }

    val methodHeader = List(
      AccessFlag.extractMethodAccessFlags(method.access).mkString(" "),
      tsv.getReturnType,
      method.name + tsv.getDeclaration).mkString(" ") + exceptions

//    println(methodHeader)


//    MethodTypeSignature(TypeParameters: List[TypeParam],
//                        parameterTypeSignatures: List[TypeSignature],
//                        returnType: TReturnable,
//                        exceptions: List[CheckedException]) extends TSignature
    ???
  }


  private val fieldSignaturePattern: Regex = {
    val primitiveSignaturePattern: String = raw"B|C|D|F|I|J|S|Z"
    val classTypeSignaturePattern: String = raw"L([^/<.]+)(/[^/<.]+)*(<[^>+]>)?"  // TODO:
    val arrayTypeSignaturePattern: String = raw"placeHolder"  // TODO:
    val typeVarPattern: String = raw"T([^;])+;"  // TODO:


    ???
  }

  // TODO: extractFieldSignature --> FieldTypeSignature.apply
  def extractFieldSignature(signature: String, descriptor: String): FieldTypeSignature = {

    ???
  }
}

