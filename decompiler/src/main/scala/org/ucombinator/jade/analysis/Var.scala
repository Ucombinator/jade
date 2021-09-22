package org.ucombinator.jade.analysis

import org.objectweb.asm.tree.analysis.BasicValue
import org.ucombinator.jade.asm.Insn
import org.objectweb.asm.tree.analysis.Value

sealed abstract class Var(val name: String) extends Value {
  def basicValue: BasicValue
  override def getSize: Int = basicValue.getSize
}

// TODO: improve variable names (also use jvm bytecode debug info for variable names)

// format: off
case class ParameterVar  (basicValue: BasicValue,             local: Int  ) extends Var(f"parameterVar${local + 1}") // TODO: +1 parameter if non-static
case class ReturnVar     (basicValue: BasicValue                          ) extends Var(f"returnVar") // TODO: used only for "expected" return type
case class ExceptionVar  (basicValue: BasicValue, insn: Insn              ) extends Var(f"exceptionVar${insn.index}")
case class InstructionVar(basicValue: BasicValue, insn: Insn              ) extends Var(f"insnVar${insn.index}")
case class CopyVar       (basicValue: BasicValue, insn: Insn, version: Int) extends Var(f"copyVar${insn.index}_${version}")
case object EmptyVar                                                        extends Var(f"emptyVar") {
  override val basicValue: BasicValue = BasicValue.UNINITIALIZED_VALUE
}
case class PhiVar        (basicValue: BasicValue, insn: Insn, index: Int  , var changed: Boolean = false) extends Var(f"phiVar${insn.index}_${index}") {
// format: on
  // TODO: use private constructor to hide `changed`
  // Note that `changed` has to be in the parameters so that the analysis sees that the value has changed
  private var changedPhiVar: PhiVar = _
  def change(): PhiVar = {
    if (this.changedPhiVar != null) { this.changedPhiVar }
    else {
      this.changedPhiVar = this.copy(changed = true)
      this.changedPhiVar.changedPhiVar = this.changedPhiVar
      this.changedPhiVar
    }
  }
}
