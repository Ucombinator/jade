package org.ucombinator.jade.util

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.comments.Comment

object JavaParser {
  def setComment[A <: Node](node: A, comment: Comment): A = {
    node.setComment(comment)
    node
  }
}
