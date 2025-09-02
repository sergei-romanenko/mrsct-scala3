package mrsc.sll.test

import org.scalacheck.{ Gen => G, _ }

import mrsc.pfp.sll._

/* naive generator of SLL expressions */
object SLLGen {
  val genVar: G[Var] = G.oneOf(Var("x"), Var("y"), Var("z"))

  val genAtom: G[Ctr] = G.oneOf(Ctr("A", Nil), Ctr("B", Nil), Ctr("C", Nil))

  val terminalTerm: G[Expr] = G.oneOf(genVar, genAtom)

  def term(depth: Int): G[Expr] =
    if (depth == 0)
      terminalTerm
    else {
      G.oneOf(call(depth), consGen(depth), terminalTerm)
    }

  def consGen(depth: Int): G[Ctr] = for {
    head <- terminalTerm
    tail <- term(depth - 1)
  } yield Ctr("Cons", List(head, tail))

  def call(depth: Int): G[Expr] = for {
    c <- G.oneOf("f", "g")
    arity <- G.choose(1, 3)
    args <- terms(arity, depth - 1)
    cname = c + arity
  } yield if (c == "f") { FCall(cname, args) } else { GCall(cname, args) }

  def terms(n: Int, depth: Int): G[List[Expr]] = G.listOfN(n, term(depth))

  implicit def arbitraryExpr: Arbitrary[Expr] =
    Arbitrary { G.sized { depth => term(3) } }
}