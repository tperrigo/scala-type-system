package tperrigo
package typelevel

// Value-level programming
sealed trait BoolVal {
  def not: BoolVal
  def or(that: BoolVal): BoolVal
}
case object True extends BoolVal {
  val not = False
  def or(that: BoolVal) = True
}
case object False extends BoolVal {
  val not = True
  def or(that: BoolVal) = that
}

/**
 * Type-level Version
 * NOTE: Must enable higher-kinded types either through the "-language:higherKinds" compiler flag,
 * or by importing "scala.language.higherKinds"
 */
sealed trait BoolType {
  type Not <: BoolType
  type Or[That <: BoolType] <: BoolType
}
sealed trait TrueType extends BoolType {
  type Not = FalseType
  type Or[That <: BoolType] = TrueType
}
sealed trait FalseType extends BoolType {
  type Not = TrueType
  type Or[That <: BoolType] = That
}

// Simple test-- at the value level, many errors can only be caught at runtime,
// while type-level problems will be caught by the compiler, so if there is an
// error, the program will fail to compile
object BoolTypeSpecs {
  implicitly[TrueType =:= TrueType]
  implicitly[FalseType =:= FalseType]

  implicitly[TrueType#Not =:= FalseType]
  implicitly[FalseType#Not =:= TrueType]

  implicitly[TrueType#Or[TrueType] =:= TrueType]
  implicitly[TrueType#Or[FalseType] =:= TrueType]
  implicitly[FalseType#Or[TrueType] =:= TrueType]
  implicitly[FalseType#Or[FalseType] =:= FalseType]

  // Testing Failure Conditions with Shapeless:
  // The macro will cause the test to compile (and pass) only if the expressions fail to compile:
  import shapeless.test.illTyped
  illTyped("implicitly[TrueType =:= FalseType]")
  illTyped("implicitly[FalseType =:= TrueType]")
  illTyped("implicitly[TrueType#Not =:= TrueType]")
  illTyped("implicitly[FalseType#Not =:= FalseType]")
  illTyped("implicitly[TrueType#Or[TrueType] =:= FalseType]")
  illTyped("implicitly[TrueType#Or[FalseType] =:= FalseType]")
}