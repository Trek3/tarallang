import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

class Program(val stmts : List[Statement], val funcs : List[Function]) {
}

class Function(val name : String, val args : List[String], val body : List[Statement], val ret : Expr)

class Scope(val name : String, val parent : Scope) {
  var variables = new HashMap[String, Expr]
}

trait Statement

case class VariableDeclaration(val name : String, val value : Expr) extends Statement
case class PrintStatement(val variable : Identifier) extends Statement

class Expr

case class Operator(val op : String, var left : Expr, var right : Expr) extends Expr
case class Identifier(name : String) extends Expr
case class Number(val value : Double) extends Expr

case class FunctionCall(name : String, val args : List[Expr]) extends Expr with Statement

case class StudentiCheParlano() extends Expr

class TarallangParser extends JavaTokenParsers{

  def program : Parser[Program] = rep(function) ~ main ^^ {
    case f ~ s => new Program(s, f)
  }

  def main : Parser[List[Statement]] = ("passiamo" ~> "all'argomento" ~> "principale" ~> codeblock <~ "." <~ "chiudete" <~ "la" <~ "porta" <~ "per" <~ "favore?") ^^ { body => body }

  def function : Parser[Function] = functionSignature ~ opt(functionArguments) ~ functionBody ~ (opt(returnStatement) <~ "ok?") ^^ {
    case name ~ args ~ body ~ None => {
      new Function(name, args.get, body, new StudentiCheParlano())
    }
    case name ~ args ~ body ~ Some(ret) => {
      new Function(name, args.get, body, ret)
    }
  }

  def functionBody : Parser[List[Statement]] = ("tale" ~> "che" ~> codeblock <~ ".") ^^ {
    s => s
  }

  def functionSignature : Parser[String] = ("prendiamo" ~> """una?""".r ~> ident) ^^ {
    name => name
  }

  def functionArguments = "con" ~> repsep(ident, "e") ^^ {
    _ toList
  }

  def returnStatement : Parser[Expr] = ("ora" ~> "abbiamo" ~> expr) ^^ {
    ret => ret
  }

  def codeblock : Parser[List[Statement]] = rep(statement) ^^ {stmt => stmt}

  def statement : Parser[Statement] = (variableDeclaration | variableAssignment | printStatement | functionCall) ^^ {stmt => stmt}

  def variableDeclaration : Parser[VariableDeclaration] = ("definisco" ~> ident) ~ "=" ~ expr ^^ {
    case name ~ "=" ~ value => {
      new VariableDeclaration(name, value)
    }
  }

  def variableAssignment : Parser[VariableDeclaration] = ("chiamo" ~> ident <~ "come") ~ (functionCall) ^^ {
    case name ~ value => {
      new VariableDeclaration(name, value)
    }
  }

  def printStatement : Parser[PrintStatement] = "vedete" ~> ident <~ "anche" <~ "in" <~ "fondo" <~ "all'aula?" ^^ {
    case msg => {
      new PrintStatement(new Identifier(msg))
    }
  }

  def number = (floatingPointNumber | wholeNumber) ^^ {
    case a => {
      new Number(a.toDouble)
    }
  }

  def functionCall : Parser[FunctionCall] = ident ~ opt("di" ~> functionCallArguments) ^^ {
    case name ~ args => {
      new FunctionCall(name, args.getOrElse(Nil))
    }
  }

  def functionCallArguments = repsep(expr, "e") ^^ {
    _ toList
  }

  def expr : Parser[Expr] = term ~ rep( ("+" | "-") ~ term) ^^ {
    case a ~ List() => a
    case a ~ b => {
      def appendExpression(c: Operator, p: Operator): Operator = {
        p.left = c
        p
      }

      var root: Operator = new Operator(b.head._1, a, b.head._2)

      for (f <- b.tail) {
        var parent =
          f._1 match {
            case "+" => new Operator("+", null, f._2)
            case "-" => Operator("-", null, f._2)
          }

        root = appendExpression(root, parent)
      }

      root
    }
  }

  def term = number | ident ^^ { new Identifier(_)}

  def parse(p : Parser[Any], in : String) = {
    parseAll(p, in) match {
      case Success(r, _) => r
      case x => x
    }
  }
}
