import munit.{FunSuite, ScalaCheckSuite}

class HelloWorldTest extends FunSuite with ScalaCheckSuite {

  test("Print hello world") {
    assertEquals(HelloWorld.printMyHello(), "Hello World")
  }

}
