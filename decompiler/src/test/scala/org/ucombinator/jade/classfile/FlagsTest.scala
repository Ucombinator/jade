// scalafmt: { trailingCommas = "always" }

package org.ucombinator.jade.classfile

import org.scalatest.freespec.AnyFreeSpec
import org.ucombinator.jade.classfile.Flags._

class FlagsTest extends AnyFreeSpec {
  "classFlags" in {
    val flags = List(
      ACC_PUBLIC,
      ACC_FINAL,
      ACC_SUPER,
      ACC_INTERFACE,
      ACC_ABSTRACT,
      ACC_SYNTHETIC,
      ACC_ANNOTATION,
      ACC_ENUM,
      ACC_MODULE,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.classFlags(flagValues.fold(0) { _ | _ }) }
  }

  "fieldFlags" in {
    val flags = List(
      ACC_PUBLIC,
      ACC_PRIVATE,
      ACC_PROTECTED,
      ACC_STATIC,
      ACC_FINAL,
      ACC_VOLATILE,
      ACC_TRANSIENT,
      ACC_SYNTHETIC,
      ACC_ENUM,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.fieldFlags(flagValues.fold(0) { _ | _ }) }
  }

  "methodFlags" in {
    val flags = List(
      ACC_PUBLIC,
      ACC_PRIVATE,
      ACC_PROTECTED,
      ACC_STATIC,
      ACC_FINAL,
      ACC_SYNCHRONIZED,
      ACC_BRIDGE,
      ACC_VARARGS,
      ACC_NATIVE,
      ACC_ABSTRACT,
      ACC_STRICT,
      ACC_SYNTHETIC,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.methodFlags(flagValues.fold(0) { _ | _ }) }
  }

  "nestedClassFlags" in {
    val flags = List(
      ACC_PUBLIC,
      ACC_PRIVATE,
      ACC_PROTECTED,
      ACC_STATIC,
      ACC_FINAL,
      ACC_INTERFACE,
      ACC_ABSTRACT,
      ACC_SYNTHETIC,
      ACC_ANNOTATION,
      ACC_ENUM,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.nestedClassFlags(flagValues.fold(0) { _ | _ }) }
  }

  "parameterFlags" in {
    val flags = List(
      ACC_FINAL,
      ACC_SYNTHETIC,
      ACC_MANDATED,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.parameterFlags(flagValues.fold(0) { _ | _ }) }
  }

  "moduleFlags" in {
    val flags = List(
      ACC_OPEN,
      ACC_SYNTHETIC,
      ACC_MANDATED,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.moduleFlags(flagValues.fold(0) { _ | _ }) }
  }

  "requiresFlags" in {
    val flags = List(
      ACC_TRANSITIVE,
      ACC_STATIC_PHASE,
      ACC_SYNTHETIC,
      ACC_MANDATED,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.requiresFlags(flagValues.fold(0) { _ | _ }) }
  }

  "exportsFlags" in {
    val flags = List(
      ACC_SYNTHETIC,
      ACC_MANDATED,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.exportsFlags(flagValues.fold(0) { _ | _ }) }
  }

  "opensFlags" in {
    val flags = List(
      ACC_SYNTHETIC,
      ACC_MANDATED,
    )
    val flagValues = flags.map(_.value)
    assert(flagValues == flagValues.distinct)
    assertResult(flags) { Flags.opensFlags(flagValues.fold(0) { _ | _ }) }
  }
}
