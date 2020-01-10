import scala.collection.breakOut
import scala.collection.mutable.HashMap

class Interpreter(program : Program) {

  var scope : Scope = new Scope("global", null)

  def run() = {
    walk(program.stmts)
  }

  def getVar(ident : Identifier) : Expr = {

    var lastScope = scope

    while(!scope.variables.contains(ident.name) && !scope.name.equals("global")){
      scope = scope.parent
    }
    if(scope.variables.contains(ident.name)){
      val value = scope.variables(ident.name)
      scope = lastScope
      return value
    }
    else
      sys.error("error: variable " + ident.name + " not found")
  }

  def eval(e : Expr) : Expr = {
    e match {
      case Number(a) => Number(a)
      case Identifier(name) => eval(getVar(e.asInstanceOf[Identifier]))
      case Operator(op, Number(left), Number(right)) => {
        op match {
          case "+" => Number(left + right)
          case "-" => Number(left - right)
          case "*" => Number(left * right)
          case "/" => Number(left / right)
        }
      }
      case Operator(op, left, right) => {
        eval(Operator(op, eval(left), eval(right)))
      }
    }
  }

  def executeFunction(f : Function, args : List[Expr]) = {

    scope = new Scope(f.name, scope)

    if(f.args.size != args.size){
      sys.error(f.name + ": wrong number of arguments, " + f.args.size + " expected.")
    }
    else{
      scope.variables = (f.args zip (args.map(x => eval(x))))(breakOut) : HashMap[String, Expr]
    }

    walk(f.body)

  }

  def walk(tree : List[Statement]) : Unit = {
    if(!tree.isEmpty) {
      tree.head match {
        case FunctionCall(name, args) => {
          val f = program.funcs.filter(x => x.name == name)

          if(f.size < 1) sys.error("function " + name + "not found.")
          else{
            executeFunction(f(0), args)

            scope = scope.parent
          }

          walk(tree.tail)
        }
        case VariableDeclaration(name, value) => {
          if(value.isInstanceOf[FunctionCall]){
            val f = program.funcs.filter(x => x.name == value.asInstanceOf[FunctionCall].name)

            if(f.size < 1) sys.error("function " + name + " not found.")
            else{
              println("bef: "+scope.name + " - " + scope.variables)
              executeFunction(f(0), value.asInstanceOf[FunctionCall].args)
              println("in: "+scope.name + " - " + scope.variables)

              scope.parent.variables(name) = eval(f(0).ret)

              scope = scope.parent

              println("aft: "+scope.name + " - " + scope.variables)

            }
          }
          else{
            scope.variables(name) = value
          }
          walk(tree.tail)
        }
        case PrintStatement(msg) => {
          println(eval(msg))

          walk(tree.tail)
        }
        case _ => ()
      }
    }
  }
}
