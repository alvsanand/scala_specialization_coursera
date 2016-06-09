package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { case (name, expr) => {
      (name, Var({
        lazy val lazyExpr = expr()
        eval(lazyExpr, namedExpressions)
      }))
    }
    }.toMap
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    evalIter(expr, references, Set.empty[String])
  }

  private def evalIter(expr: Expr, references: Map[String, Signal[Expr]], previousReferences: Set[String]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) if !previousReferences.contains(name) => evalIter(getReferenceExpr(name, references),
        references, previousReferences + name)
      case Plus(a: Expr, b: Expr) => {
        val aVal = evalIter(a, references, previousReferences)
        val bVal = evalIter(b, references, previousReferences)
        aVal + bVal
      }
      case Minus(a: Expr, b: Expr) => {
        val aVal = evalIter(a, references, previousReferences)
        val bVal = evalIter(b, references, previousReferences)
        aVal - bVal
      }
      case Times(a: Expr, b: Expr) => {
        val aVal = evalIter(a, references, previousReferences)
        val bVal = evalIter(b, references, previousReferences)
        aVal * bVal
      }
      case Divide(a: Expr, b: Expr) => {
        val aVal = evalIter(a, references, previousReferences)
        val bVal = evalIter(b, references, previousReferences)
        aVal / bVal
      }
      case _ => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
