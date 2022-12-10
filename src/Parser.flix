pub type alias Input[a] = List[a]

pub type alias ParseResult[a, b] = List[(a, Input[b])]

pub type alias Parser[a, b] = Input[b] -> ParseResult[a, b]

namespace Parser {

    ///
    /// Returns a non-empty `ParseResult` with value `b`
    /// regardless of input.
    ///
    pub def succeed(a: a, inp: Input[b]): ParseResult[a, b] =
        (a, inp) :: Nil

    ///
    /// Returns an empty `ParseResult` regardless of input.
    ///
    pub def fail(_: Input[b]): ParseResult[a, b] =
        Nil

    ///
    /// Returns a non-empty `ParseResult` if the input is non-empty
    /// and the first element satisfies the predicate `p`.
    /// Returns an empty `ParseResult` otherwise.
    ///
    pub def satisfy(p: a -> Bool \ ef, inp: Input[a]): ParseResult[a, a] \ ef = match inp {
        case Nil             => fail(inp)
        case x :: xs if p(x) => succeed(x, xs)
        case _ :: xs         => fail(xs)
    }

}
