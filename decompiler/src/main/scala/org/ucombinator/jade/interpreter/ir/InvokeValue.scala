package org.ucombinator.jade.interpreter.ir

import org.objectweb.asm.Opcodes


/** Invoke */
abstract class InvokeValue(obj: Value, method: String, parameters: List[Value])
    extends Value {
  require(isReference(obj))
}

object InvokeValue {
  def of(opcode: Int)(obj: Value, method: String, parameters: List[Value]): InvokeValue =
    opcode match {
      case Opcodes.INVOKEVIRTUAL   => InvokeVirtualV(obj, method, parameters)
      case Opcodes.INVOKESPECIAL   => InvokeSpecialV(obj, method, parameters)
      case Opcodes.INVOKESTATIC    => InvokeStaticV(obj.asInstanceOf[ClassV], method, parameters)
      case Opcodes.INVOKEINTERFACE => InvokeInterfaceV(obj, method, parameters)
    }
}

case class InvokeVirtualV(instance: Value, method: String, parameters: List[Value])
  extends InvokeValue(instance, method, parameters) {
  require(isReference(instance))
}

case class InvokeSpecialV(instance: Value, method: String, parameters: List[Value])
  extends InvokeValue(instance, method, parameters) {
  require(isReference(instance))
}

case class InvokeStaticV(cls: ClassV, method: String, parameters: List[Value])
  extends InvokeValue(cls, method, parameters)

// TODO: Add a field for interface info for future type inference
case class InvokeInterfaceV(instance: Value, method: String, parameters: List[Value])
  extends  InvokeValue(instance, method, parameters)

// TODO: `case class InvokeDynamicV`
//case object InvokeDynamicV extends InvokeValue

