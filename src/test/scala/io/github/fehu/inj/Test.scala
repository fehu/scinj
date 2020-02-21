package io.github.fehu.inj


trait A
class A1(val x: Int) extends A {
  override def toString: String = s"A($x)"
}

trait B
class B1(val s: String) extends B {
  override def toString: String = s"B($s)"
}

trait C
class C1(val s: String) extends C {
  override def toString: String = s"C($s)"
}


@module(log = true)
class TestModule extends Module {

  lazily   bind[A] new A1(5)
  strictly bind[B] new B1("foo")

}

@injectable
class TestServiceA(implicit val injector: Injector.Aux[TestServiceA#InjectionDependencies]) extends Injectable {
  val a = inject[A]
}

@injectable
class TestServiceB(implicit val injector: Injector.Aux[TestServiceB#InjectionDependencies]) extends Injectable {
  val b = inject[B]
}

@injectable
class TestServiceC(implicit val injector: Injector.Aux[TestServiceC#InjectionDependencies]) extends Injectable {
  val c = inject[C]
}


object Test extends App {
  implicit val m = new TestModule

  val sa = new TestServiceA
  println(sa.a)

  val sb = new TestServiceB
  println(sb.b)

  shapeless.test.illTyped("new TestServiceC", "Not found injector providing io.github.fehu.inj.C :: shapeless.HNil")
}