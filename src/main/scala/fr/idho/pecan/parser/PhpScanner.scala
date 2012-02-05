package fr.idho.pecan.parser
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

class PhpScanner(val startInline: Boolean = true) extends StdLexical with PhpTokens {
  delimiters ++= ("- + * / %"
    + " ++ -- -= += *= /= %="
    + " & | ^ >> <<"
    + " &= |= ^= >>= <<="
    + " . $"
    + " == === != <> !== < > <= >="
    + " = ; , ( ) [ ] { } -> => \\").split(" ")

  reserved ++= Array(
    //Keywords
    "abstract", //PHP 5+
    "and",
    "array",
    "as",
    "break",
    "case",
    "catch", //PHP 5+
    "cfunction", //PHP4
    "class",
    "clone", //PHP 5+
    "const",
    "continue",
    "declare",
    "default",
    "do",
    "else",
    "elseif",
    "enddeclare",
    "endfor",
    "endforeach",
    "endif",
    "endswitch",
    "endwhile",
    "extends",
    "final", //PHP 5+
    "for",
    "foreach",
    "function",
    "global",
    "goto", //PHP 5.3+
    "if",
    "implements", //PHP 5+
    "interface", //PHP 5+
    "instanceof", //PHP 5+
    "namespace", //PHP 5.3+
    "new",
    "old_function", //PHP 4
    "or",
    "private", //PHP 5+
    "protected", //PHP 5+
    "public", //PHP 5+
    "static",
    "switch",
    "throw", //PHP 5+
    "try", //PHP 5+
    "use",
    "var",
    "while",
    "xor",
    //Compile-time constants
    "__CLASS__",
    "__DIR__", //PHP 5.3+
    "__FILE__",
    "__LINE__",
    "__FUNCTION__",
    "__METHOD__",
    "__NAMESPACE__", //PHP 5.3+
    //Language constructs
    "die",
    "echo",
    "empty",
    "exit",
    "eval",
    "include",
    "include_once",
    "isset",
    "list",
    "require",
    "require_once",
    "return",
    "print",
    "unset")

  override def token = {
    inlineHtml | super.token
  }

  def inlineHtml: Parser[Token] = {
    //PHP files start with inline HTML
    val first = (html <~ (phpOpenTag | EofCh) ^^ { new InlineHtml(_) })
    val second = (phpCloseTag ~> html <~ (phpOpenTag | EofCh) ^^ { new InlineHtml(_) })
    new Parser[Token] {
      def apply(in: Input) = {
        if (startInline && in.offset == 0)
          first(in)
        else
          second(in)
      }
    }
  }

  def phpOpenTag = '<' ~ '?' ~ 'p' ~ 'h' ~ 'p'
  def phpCloseTag = '?' ~ '>'

  private def html = rep(not(phpOpenTag) ~> chrExcept(EofCh)) ^^ { _ mkString "" }
}
