package io.github.fehu.inj

import scala.annotation.{ StaticAnnotation, compileTimeOnly, implicitNotFound }

import io.github.fehu.inj.macros._


trait Injector {
  import scala.language.experimental.macros

  type Injections

  def injectUnsafe(id: String): Option[Any]

  final def inject[T]: T = macro InjectorMacros.inject[T]
}

object Injector {
  import scala.language.experimental.macros

  @implicitNotFound("Not found injector providing ${I}")
  type Aux[I] = Injector { type Injections = I }

  implicit def injectorConversion[I, R](implicit inj: Injector.Aux[I], convert: Convert[I, R]): Injector.Aux[R] = convert(inj)

  trait Convert[From, To] {
    def apply(from: Injector.Aux[From]): Injector.Aux[To]
  }
  object Convert {
    implicit def convertInjector[From, To]: Convert[From, To] = macro InjectorMacros.convert[From, To]
  }
}


@compileTimeOnly("enable macro paradise to expand macro annotations")
class injectable(log: Boolean = false) extends StaticAnnotation {
  import scala.language.experimental.macros

  def macroTransform(annottees: Any*): Any = macro InjectorMacros.injectableTransform
}

trait Injectable {
  type InjectionDependencies

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

trait Module extends Injector {
  final def injector: Injector.Aux[Injections] = this

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
