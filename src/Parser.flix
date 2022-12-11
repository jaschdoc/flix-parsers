/*
   Copyright 2022 jaschdoc

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

pub type alias Input[a] = List[a]

pub type alias ParseResult[a, b] = List[(a, Input[b])]

pub type alias Parser[a, b] = Input[b] -> ParseResult[a, b]

namespace Parser {

    ///
    /// Returns a non-empty `ParseResult` with value `b`
    /// regardless of input.
    ///
    pub def succeed(a: a, input: Input[b]): ParseResult[a, b] =
        (a, input) :: Nil

    ///
    /// Returns an empty `ParseResult` regardless of input.
    ///
    /// Equivalent to the empty string ϵ.
    ///
    pub def fail(_: Input[b]): ParseResult[a, b] =
        Nil

    ///
    /// Returns a non-empty `ParseResult` if the input is non-empty
    /// and the first element satisfies the predicate `p`.
    /// Returns an empty `ParseResult` otherwise.
    ///
    pub def satisfy(p: a -> Bool, input: Input[a]): ParseResult[a, a] = match input {
        case Nil             => fail(input)
        case x :: xs if p(x) => succeed(x, xs)
        case _ :: xs         => fail(xs)
    }

    ///
    /// Returns a non-empty `ParseResult` if the first element of the input
    /// is equal to `a`.
    /// Returns an empty ParseReult otherwise.
    ///
    pub def literal(a: a, input: Input[a]): ParseResult[a, a] with Eq[a] =
        satisfy(Eq.eq(a) , input)

    ///
    /// Returns the result of parser `p1` applied to `input`
    /// followed by the result of parser `p2` applied to `input`.
    ///
    pub def either(p1: Parser[a, b], p2: Parser[a, b], input: Input[b]): ParseResult[a, b] =
        p1(input) ::: p2(input)

    ///
    /// Returns the `ParseResult` of sequencing `p1`, `p2`, i.e.
    /// `p1` is applied to the input and `p2` is applied to the
    /// input that `p1` did not consume.
    ///
    /// Equivalent to `AB` where `A` and `B` are non-terminals.
    ///
    pub def then(p1: Parser[a, b], p2: Parser[c, b], input: Input[b]): ParseResult[(a, c), b] =
        for (
            (x1, rest1) <- p1(input);
            (x2, rest2) <- p2(rest1)
        ) yield ((x1, x2), rest2)

    ///
    /// Returns the result of applying `f` to every element of
    /// the `ParseResult` of `p` on `input`.
    ///
    pub def using(p: Parser[a, b], f: a -> c, input: Input[b]): ParseResult[c, b] =
        for (
            (x, rest) <- p(input)
        ) yield (f(x), rest)

    ///
    /// Returns zero or more results of the parser `p`.
    ///
    /// Note that it always succeeds, so the result will always
    /// be non-empty, but the rest of the input may be `Nil`, along
    /// with the result. I.e. both the value of `List[a]` may be `Nil`
    /// and `b` may be `Nil`.
    ///
    /// Equivalent to `A*`.
    ///
    pub def many(p: Parser[a, b], input: Input[b]): ParseResult[List[a], b] =
        let p1 = (p `then` many(p)) `using` cons;
        either(p1, succeed(Nil), input)

    ///
    /// Returns one or more results of the parser `p`.
    ///
    /// Note that unlike `many`, this parser may fail, i.e.
    /// return 0 results.
    ///
    /// Equivalent to `A+`.
    ///
    pub def some(p: Parser[a, b], input: Input[b]): ParseResult[List[a], b] =
        ((p `then` many(p)) `using` cons)(input)

    ///
    /// Returns one or more `ParseResults` recognizing numbers.
    /// The longest match will be the first result.
    ///
    pub def number(input: Input[Char]): ParseResult[List[Char], Char] =
        let digit = c -> '0' <= c and c <= '9';
        input |> some(satisfy(digit))

    ///
    /// Returns the string `s` as an `Input` type.
    ///
    pub def inputFromString(s: String): Input[Char] =
        String.toList(s)

    ///
    /// Returns the tuple as a list, i.e.
    /// `(x, xs)` is returned as `x :: xs`.
    ///
    def cons(xs: (a, List[a])): List[a] =
        fst(xs) :: snd(xs)

}
