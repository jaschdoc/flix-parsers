type alias Parser[a, b] = List[b] -> List[(a, List[b])]

namespace Parser {

    ///
    /// Returns a parser that succeeds with value `b`,
    /// regardless of input.
    ///
    pub def succeed(a: a): Parser[a, b] =
        inp -> (a, inp) :: Nil

    ///
    /// Returns a parser that fails regardless of input.
    ///
    pub def fail(): Parser[a, b] =
        _ -> Nil

}
