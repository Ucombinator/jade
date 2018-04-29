package org.ucombinator.jade.ir

abstract class CompareValue extends Value with BooleanT

// TODO:
// TODO:
// TODO:
// TODO: should be `case class`

/** from Comparisons -- JVMS page 568 */
case object LcmpV      extends CompareValue  // 148
case object FcmplV     extends CompareValue  // 149
case object FcmpgV     extends CompareValue  // 150
case object DcmplV     extends CompareValue  // 151
case object DcmpgV     extends CompareValue  // 152
case object IfeqV      extends CompareValue  // 153
case object IfneV      extends CompareValue  // 154
case object IfltV      extends CompareValue  // 155
case object IfgeV      extends CompareValue  // 156
case object IfgtV      extends CompareValue  // 157
case object IfleV      extends CompareValue  // 158
case object If_icmpqV  extends CompareValue  // 159
case object If_icmpneV extends CompareValue  // 160
case object If_icmpltV extends CompareValue  // 161
case object If_icmpgeV extends CompareValue  // 162
case object If_icmpgtV extends CompareValue  // 163
case object If_icmpleV extends CompareValue  // 164
case object If_acmpeqV extends CompareValue  // 165
case object If_acmpneV extends CompareValue  // 166


/** from Extended -- JVMS page 568 */
case object IfnullV    extends CompareValue  // 198
case object IfnonnullV extends CompareValue  // 199


