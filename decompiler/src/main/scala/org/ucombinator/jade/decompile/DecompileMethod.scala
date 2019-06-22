package org.ucombinator.jade.decompile

import com.github.javaparser.ast.stmt.BlockStmt
import org.objectweb.asm.tree.{ClassNode, MethodNode}

object DecompileMethod {
  def decompileBody(classNode: ClassNode, node: MethodNode): BlockStmt = {
    node.instructions
    ???
  }
}
