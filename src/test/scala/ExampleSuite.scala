import org.scalatest.FunSuite
import fr.idho.pecan.parser.PhpParser
import fr.idho.pecan.parser.PhpScanner

class ExampleSuite extends FunSuite {
  val parser = new PhpParser(new PhpScanner(false))
  
  test("Valid PHP file") {
    assert(parser parse "if (true);" successful)
  }

  test("Invalid PHP file") {
      assert(false === (parser parse "if" successful))
  }
}