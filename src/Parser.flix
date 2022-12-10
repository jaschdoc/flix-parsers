type alias Parser[a, b] = (List[a] -> List[(b, List[a])]) // Use a -> List[Validation[b, a]]

namespace Parser {

    ///
    /// Returns a parser that succeeds with value `b`,
    /// regardless of input.
    ///
    pub def succeed(b: b): Parser[a, b] =
        inp -> (b, inp) :: Nil

    ///
    /// Returns a parser that fails regardless of input.
    ///
    pub def fail(): Parser[a, b] =
        _ -> Nil

}
