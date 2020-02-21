package io.github.fehu.inj.macros

import scala.reflect.macros.whitebox

import shapeless.CaseClassMacros

class InjectorMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def moduleTransform(annottees: Tree*): Tree = {
    val transformed = annottees.map{
      case ClassDef(cmod, cname, ctps, Template(cparents, cself, body)) =>
        val (newBody, bound) = ModuleTreeTransformer(body)
        val injections = q"type Injections = ${mkHListTpe(bound)}"
        val injectCases = bound.map { tpe =>
          cq"${tpe.typeSymbol.fullName} => this.${boundName(tpe)}"
        }
        val inject = q"def injectUnsafe(id: String): Option[Any] = PartialFunction.condOpt(id) { case ..$injectCases }"
        ClassDef(cmod, cname, ctps, Template(cparents, cself, newBody ::: List(injections, inject)))
      case other => other
    }
    q"..$transformed"
  }

  protected object ModuleTreeTransformer {
    def apply(trees: List[Tree]): (List[Tree], List[Type]) = {
      val trans = new ModuleTreeTransformer
      val t = trans.transformTrees(trees)
      (t, trans.bound)
    }
  }
  protected class ModuleTreeTransformer extends Transformer {
    var bound: List[Type] = Nil

    override def transform(tree: Tree): Tree = tree match {
      case q"$bindM.bind[$tpe0]($bindV)" =>
        val tpe = safeType(tpe0)
        bound ::= tpe
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
    val transformed = annottees.map{
      case ClassDef(cmod, cname, ctps, Template(cparents, cself, body)) =>
        val (newBody, injected) = InjectableTreeTransformer(body)
        val dependencies = q"type InjectionDependencies = ${mkHListTpe(injected)}"
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
    val bindings = unpackHListTpe(c.prefix.tree.tpe.decl(TypeName("Injections")).typeSignature)
    val tpe = weakTypeOf[T]
    if (!bindings.contains(tpe)) c.abort(c.enclosingPosition, s"No injection is defined for $tpe")
    else q"${c.prefix}.injectUnsafe(${tpe.typeSymbol.fullName}).get.asInstanceOf[$tpe]"
  }


  def convert[From: WeakTypeTag, To: WeakTypeTag]: Tree = {
    val From = weakTypeOf[From]
    val To   = weakTypeOf[To]

    val bindingsFrom = unpackHListTpe(From)
    val bindingsTo   = unpackHListTpe(To)

    if (bindingsTo.forall(bindingsFrom.contains))
      q"""
        new _root_.io.github.fehu.inj.Injector.Convert[$From, $To] {
          def apply(from: Injector.Aux[$From]): Injector.Aux[$To] = from.asInstanceOf[Injector.Aux[$To]]
        }
       """
    else c.abort(c.enclosingPosition, s"Injector $From cannot be converted to $To")
  }

}

