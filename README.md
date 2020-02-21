# Yet another dependency injections for Scala

Module bindings are collected and encoded at type level.
```scala
@module
class TestModule extends Module {
  lazily   bind[A] new A1(5)
  strictly bind[B] new B1("foo")
  
  // Generated:
  type Injections = A :: B :: HNil
  def injectUnsafe(id: String): Option[Any] = ...
}
```

Used injections are collected and encoded at type level.
```scala
@injectable
class TestService(implicit val injector: Injector.Aux[TestService#InjectionDependencies]) extends Injectable {
  val a = inject[A]
  val b = inject[B]
  
  // Generated:
  type InjectionDependencies = A :: B :: HNil
}
```

When initializing the service, the existence of all the dependencies in the module is checked.
```scala
  implicit val m = new TestModule
  val s = new TestService
```
