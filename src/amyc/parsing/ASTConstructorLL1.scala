package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match {
      case Node('QName ::= _, List(id, qn2)) =>
        val (id1, pos) = constructName(id)
        constructQname2(qn2, id1, pos)
    }
  }

  def constructQname2(pTree: NodeOrLeaf[Token], id1: String, pos: Positioned): (QualifiedName, Positioned) = {
    pTree match {
      case Node('QName2 ::= (DOT() :: _), List(_, id)) =>
        (QualifiedName(Some(id1), constructName(id)._1), pos)
      case Node('QName2 ::= _, List()) =>
        (QualifiedName(None, id1), pos)
    }
  }

  override def constructExpr(pTree: NodeOrLeaf[Token]): Expr = {
    pTree match {
      case Node('Expr10 ::= (ERROR() :: _), List(Leaf(ert), _, msg, _)) =>
        Error(constructExpr(msg)).setPos(ert)
      case Node('Expr10 ::= (IF() :: _), List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)) =>
        Ite(
          constructExpr(cond),
          constructExpr(thenn),
          constructExpr(elze)
        ).setPos(it)
      case Node('Expr10 ::= ('Id :: _), List(id, seq)) =>
        val (name, pos) = constructName(id)
        constructIdSeq(seq, name, pos)
      case Node('Expr10 ::= List('LiteralExp), List(lit)) =>
        constructLiteralExp(lit)
      case Node('Expr10 ::= (LPAREN() :: _), List(Leaf(lp), seq)) =>
        constructLParenSeq(seq, lp)
      case Node('Expr9 ::= (BANG() :: _), List(Leaf(bt), e10)) =>
        Not(constructExpr(e10)).setPos(bt)
      case Node('Expr9 ::= (MINUS() :: _), List(Leaf(mt), e10)) =>
        Neg(constructExpr(e10)).setPos(mt)
      case Node('Expr9 ::= _, List(e10)) =>
        constructExpr(e10)
      case Node('Expr8 ::= _, List(e9, seq)) =>
        constructOpExpr(constructExpr(e9), seq)
      case Node('Expr7 ::= _, List(e8, seq)) =>
        constructOpExpr(constructExpr(e8), seq)
      case Node('Expr6 ::= _, List(e7, seq)) =>
        constructOpExpr(constructExpr(e7), seq)
      case Node('Expr5 ::= _, List(e6, seq)) =>
        constructOpExpr(constructExpr(e6), seq)
      case Node('Expr4 ::= _, List(e5, seq)) =>
        constructOpExpr(constructExpr(e5), seq)
      case Node('Expr3 ::= _, List(e4, seq)) =>
        constructOpExpr(constructExpr(e4), seq)
      case Node('Expr2 ::= _, List(e3, seq)) =>
        val expr = constructExpr(e3)
        constructExpr2Seq(seq, expr)
      case Node('Expr ::= (VAL() :: _), List(Leaf(vt), param, _, value, _, body)) =>
        Let(constructParam(param), constructExpr(value), constructExpr(body)).setPos(vt)
      case Node('Expr ::= _, List(e2, seq)) =>
        val expr1 = constructExpr(e2)
        constructExprSeq(seq, expr1)
    }
  }

  def constructExprSeq(pTree: NodeOrLeaf[Token], expr1: Expr): Expr = {
    pTree match {
      case Node('ExprSeq ::= (SEMICOLON() :: _), List(_, expr2)) =>
        Sequence(expr1, constructExpr(expr2)).setPos(expr1)
      case Node('ExprSeq ::= _, List()) => expr1
    }
  }

  def constructExpr2Seq(pTree: NodeOrLeaf[Token], expr: Expr): Expr = {
    pTree match {
      case Node('Expr2Seq ::= (MATCH() :: _), List(_, _, cases, _)) =>
        Match(expr, constructCases(cases))
      case Node('Expr2Seq ::= _, List()) => expr
    }
  }

  def constructCases(pTree: NodeOrLeaf[Token]): List[MatchCase] = {
    pTree match {
      case Node('Cases ::= _, List(cas, cases)) =>
        cases match {
          case Node('Cases2 ::= List('Cases), List(casesSeq)) =>
            constructCase(cas) :: constructCases(casesSeq)
          case Node('Cases2 ::= _, List()) =>
            List(constructCase(cas))
        }
    }
  }

  def constructIdSeq(pTree: NodeOrLeaf[Token], id: String, pos: Positioned): Expr = {
    pTree match {
      case Node('IdSeq ::= ('QName2 :: _), List(qn, _, as, _)) =>
        val qname = constructQname2(qn, id, pos)._1
        val args = constructList(as, constructExpr, hasComma = true)
        Call(qname, args).setPos(pos)
      case Node('IdSeq ::= _, List()) =>
        Variable(id).setPos(pos)
    }
  }

  def constructLiteralExp(pTree: NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node('LiteralExp ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('LiteralExp ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('LiteralExp ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('LiteralExp ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
    }
  }

  def constructLParenSeq(pTree: NodeOrLeaf[Token], token: Token): Expr = {
    pTree match {
      case Node('LParenSeq ::= ('Expr :: _), List(expr, _)) =>
        constructExpr(expr).setPos(token)
      case Node('LParenSeq ::= _, _) =>
        UnitLiteral().setPos(token)
    }
  }

  override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= ('Id :: _), List(id, seq)) =>
        val (name, pos) = constructName(id)
        constructPatternSeq(seq, name, pos)
    }
  }

  def constructPatternSeq(pTree: NodeOrLeaf[Token], id1: String, pos: Positioned): Pattern = {
    pTree match {
      case Node('PatternIdSeq ::= ('QName2 :: _), List(qn, _, patts, _)) =>
        val qname = constructQname2(qn, id1, pos)._1
        val patterns = constructList(patts, constructPattern, hasComma = true)
        CaseClassPattern(qname, patterns).setPos(pos)
      case Node('PatternIdSeq ::= _, List()) =>
        IdPattern(id1).setPos(pos)
    }
  }

  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('OrExpr, 'AndExpr, 'EqExpr, 'CompExpr, 'AddExpr, 'MultExpr) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExpr(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }
}

