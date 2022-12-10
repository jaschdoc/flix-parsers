pub type alias Input[a] = List[a]

pub type alias ParseResult[a, b] = List[(a, Input[b])]

pub type alias Parser[a, b] = Input[b] -> ParseResult[a, b]

namespace Parser {

    ///
    /// Returns a parser that succeeds with value `b`,
    /// regardless of input.
    ///
    pub def succeed(a: a, inp: Input[b]): ParseResult[a, b] =
        (a, inp) :: Nil

    ///
    /// Returns a parser that fails regardless of input.
    ///
    pub def fail(_: Input[b]): ParseResult[a, b] =
        Nil

    pub def satisfy(f: a -> Bool \ ef, inp: Input[a]): ParseResult[a, a] \ ef = match inp {
        case Nil             => fail(inp)
        case x :: xs if f(x) => succeed(x, xs)
        case _ :: xs         => fail(xs)
    }

}
