package org.ucombinator.jade.util

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.comments.Comment
import com.github.javaparser.ast.comments.BlockComment
import com.github.javaparser.ast.stmt.EmptyStmt

object JavaParser {
  def setComment[A <: Node](node: A, comment: Comment): A = {
    node.setComment(comment)
    node
  }
  def noop(comment: String): EmptyStmt = {
    JavaParser.setComment(new EmptyStmt(), new BlockComment(comment))
  }
}
