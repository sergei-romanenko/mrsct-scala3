package mrsc.pfp.charity

// Please note that in Charity literature
// a b c d == a (b(c(d)))
// {f, g} : X -> (Y x Z)
object CharityInterpreter {
  def eval(e: Expr): Expr = (e: @unchecked) match {
    // id, 
    case FoldApp(fold, CtrApp(n, e1)) => {
      // We take the corresponding var-base and match it against e1.
      // Then we replace the things of type C with the corresponding fold.
      AbsApp(null, e1)
      null
    }
  }
}
