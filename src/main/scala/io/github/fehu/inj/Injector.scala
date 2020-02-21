package io.github.fehu.inj

import scala.annotation.{ StaticAnnotation, compileTimeOnly, implicitNotFound }

import io.github.fehu.inj.macros._


trait Injector {
  import scala.language.experimental.macros

  type Injections <: Injector.Bindings

  def injectUnsafe(id: String): Option[Any]

  final def inject[T]: T = macro InjectorMacros.inject[T]
}

object Injector {
  import scala.language.experimental.macros

  type Aux[I <: Injector.Bindings] = Injector { type Injections = I }

  implicit def fromModule[R]: Injector.Aux[R] = macro InjectorMacros.injectorFromModule[R]

  sealed trait Bindings
  case class BCons[H, T <: Bindings](head: H, tail: Bindings) extends Bindings
  case object BNil extends Bindings
}


@compileTimeOnly("enable macro paradise to expand macro annotations")
class injectable(log: Boolean = false) extends StaticAnnotation {
  import scala.language.experimental.macros

  def macroTransform(annottees: Any*): Any = macro InjectorMacros.injectableTransform
}

trait Injectable {
  type InjectionDependencies <: Injector.Bindings

  protected def injector: Injector.Aux[InjectionDependencies]

  @compileTimeOnly("You must put @injectable annotation to use injections")
  protected final def inject[T]: T = sys.error("@compileTimeOnly")

  @compileTimeOnly("You must put @injectable annotation to use injections")
  protected final def inject[T](label: String): T = sys.error("@compileTimeOnly")
}


@compileTimeOnly("enable macro paradise to expand macro annotations")
class module(log: Boolean = false) extends StaticAnnotation {
  import scala.language.experimental.macros

  def macroTransform(annottees: Any*): Any = macro InjectorMacros.moduleTransform
}

trait Module {
  type Bindings <: Injector.Bindings

  @compileTimeOnly("You must put @module annotation to bind injections")
  protected final def lazily: Binder = sys.error("@compileTimeOnly")

  @compileTimeOnly("You must put @module annotation to bind injections")
  protected final def strictly: Binder = sys.error("@compileTimeOnly")

  @compileTimeOnly("You must put @module annotation to bind injections")
  protected final def byName: Binder = sys.error("@compileTimeOnly")

  protected sealed class Binder {
    @compileTimeOnly("You must put @module annotation to bind injections")
    def bind[T](t: T): BindSetup = sys.error("@compileTimeOnly")
  }

  protected class BindSetup // TODO
}
object Module {
  type Aux[B <: Injector.Bindings] = Module { type Bindings = B }
}
