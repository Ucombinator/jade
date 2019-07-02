package org.ucombinator.jade.decompile

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.expr.NullLiteralExpr
import com.github.javaparser.ast.stmt.{BlockStmt, ThrowStmt}
import org.objectweb.asm.tree.{ClassNode, MethodNode}

object DecompileBody {
  def decompileBody(classNode: ClassNode, node: MethodNode): BlockStmt = {
    new BlockStmt(new NodeList(new ThrowStmt(new NullLiteralExpr()))) // TODO
    // TODO: node.instructions
  }
}
