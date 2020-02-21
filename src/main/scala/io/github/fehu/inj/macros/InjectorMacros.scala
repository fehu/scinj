package io.github.fehu.inj.macros

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

import io.github.fehu.inj.{ Injector, Module }

class InjectorMacros(val c: blackbox.Context) {
  import c.universe._

  def moduleTransform(annottees: Tree*): Tree = {
    // TODO: refactor
    val log = PartialFunction.cond(c.prefix.tree) { case q"new module(log = true)" => true }
    val transformed = annottees.map{
      case ClassDef(cmod, cname, ctps, Template(cparents, cself, body)) =>
        val (newBody, bound) = ModuleTreeTransformer(body)
        val bindings = q"type Bindings = ${mkBindings(bound.map(_._1))}"
        val res = ClassDef(cmod, cname, ctps, Template(cparents, cself, newBody ::: List(bindings)))
        lazy val boundInfo = bound.map { case (tpe, bound) => s"  * $tpe <- $bound" }
        if (log) c.info(c.enclosingPosition, s"$cname bindings:\n${boundInfo.mkString("\n")}", force = true)
        res
      case other => other
    }
    q"..$transformed"
  }

  protected object ModuleTreeTransformer {
    def apply(trees: List[Tree]): (List[Tree], List[(Type, Tree)]) = {
      val trans = new ModuleTreeTransformer
      val t = trans.transformTrees(trees)
      (t, trans.bound)
    }
  }
  protected class ModuleTreeTransformer extends Transformer {
    var bound: List[(Type, Tree)] = Nil

    override def transform(tree: Tree): Tree = tree match {
      case q"$bindM.bind[$tpe0]($bindV)" =>
        val tpe = safeType(tpe0)
        bound ::= tpe -> bindV
        val name = boundName(tpe)
        (bindM.toString: @unchecked) match {
           case "lazily"   => q"final lazy val $name = $bindV"
           case "strictly" => q"final val      $name = $bindV"
           case "byName"   => q"final def      $name = $bindV"
         }
      case _ =>
        super.transform(tree)
    }

  }

  protected def boundName(tpe: Type): TermName = {
    val snake_case = tpe.typeSymbol.fullName.map {
      case c if c.isLetterOrDigit => c
      case _ => '_'
    }
    TermName(s"bound_$snake_case")
  }

  protected def safeType(tree: Tree): Type =
    Option(tree.tpe).getOrElse(
      if (tree.isType) c.typecheck(q"??? : $tree").tpe
      else c.typecheck(tree).tpe
    )


  def injectableTransform(annottees: Tree*): Tree = {
    // TODO: duplication
    val log = PartialFunction.cond(c.prefix.tree) { case q"new injectable(log = true)" => true }
    val transformed = annottees.map{
      case ClassDef(cmod, cname, ctps, Template(cparents, cself, body)) =>
        val (newBody, injected) = InjectableTreeTransformer(body)
        val dependencies = q"type InjectionDependencies = ${mkBindings(injected)}"
        lazy val injectedInfo = injected.map(tpe => s"  * $tpe")
        if (log) c.info(c.enclosingPosition, s"$cname injections:\n${injectedInfo.mkString("\n")}", force = true)
        ClassDef(cmod, cname, ctps, Template(cparents, cself, newBody ::: List(dependencies)))
      case other => other
    }
    q"..$transformed"
  }

  protected object InjectableTreeTransformer {
    def apply(trees: List[Tree]): (List[Tree], List[Type]) = {
      val trans = new InjectableTreeTransformer
      val t = trans.transformTrees(trees)
      (t, trans.injected)
    }
  }
  protected class InjectableTreeTransformer extends Transformer {
    var injected: List[Type] = Nil

    override def transform(tree: Tree): Tree = tree match {
      case q"inject[$tpe0]" =>
        val tpe = safeType(tpe0)
        injected ::= tpe
        q"injector.inject[$tpe]"
      case _ => super.transform(tree)
    }
  }

  def inject[T: WeakTypeTag]: Tree = {
    val bindings = unpackBindings(c.prefix.tree.tpe.decl(TypeName("Injections")).typeSignature)
    val tpe = weakTypeOf[T]
    if (!bindings.contains(tpe)) c.abort(c.enclosingPosition, s"No injection is defined for $tpe")
    else q"${c.prefix}.injectUnsafe(${tpe.typeSymbol.fullName}).get.asInstanceOf[$tpe]"
  }


  def injectorFromModule[R: WeakTypeTag]: Tree = {
    val module = c.inferImplicitValue(typeOf[Module]) match {
      case EmptyTree => c.abort(c.enclosingPosition, s"No implicit module is in scope")
      case module    => module
    }
    val moduleBindings     = unpackBindings(module.tpe.decl(TypeName("Bindings")).typeSignature)
    val requiredInjections = unpackBindings(weakTypeOf[R])
    val lackingBindings    = requiredInjections.filterNot(moduleBindings.contains)
    if (lackingBindings.isEmpty) {
      val injectCases = requiredInjections.map { tpe => cq"${tpe.typeSymbol.fullName} => $module.${boundName(tpe)}" }
      q"""
        new _root_.io.github.fehu.inj.Injector {
          type Injections = ${weakTypeOf[R]}
          def injectUnsafe(id: String): Option[Any] = PartialFunction.condOpt(id) { case ..$injectCases }
        }
      """
    }
    else c.abort(c.enclosingPosition, s"Module $module lacks following bindings:${lackingBindings.mkString("\n  * ", "\n  * ", "")}")
  }

  protected def mkBindings(tpes: List[Type]): Type = tpes.foldLeft(BNilType) { (acc, tpe) => appliedType(BConsType, tpe, acc) }

  protected def unpackBindings(tpe0: Type): List[Type] = {
    @tailrec
    def inner(tpe: Type, accRev: List[Type]): List[Type] = tpe match {
      case TypeRef(_, BConsSymb, List(h, t)) => inner(t, h :: accRev)
      case _ => accRev.reverse
    }
    inner(tpe0.dealias, Nil)
  }

  protected val BNilType  = typeOf[Injector.BNil.type]
  protected val BConsType = typeOf[Injector.BCons[_, _]].typeConstructor
  protected val BConsSymb = symbolOf[Injector.BCons[_, _]]
}

