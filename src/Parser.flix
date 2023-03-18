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

pub type alias Input[a] = DelayList[a]

pub type alias ParseResult[a, b] = DelayList[(a, Input[b])]

pub type alias Parser[a, b] = Input[b] -> ParseResult[a, b]

mod Parser {

    use DelayList.{ENil, ECons, LCons, LList};

    ///
    /// Returns a parser that always succeeds with value `b`
    /// regardless of input.
    ///
    pub def succeed(a: a): Parser[a, b] =
        inp -> ECons((a, inp), ENil)

    ///
    /// Returns a parser that always fails regardless of input.
    ///
    /// Equivalent to the empty string ϵ.
    ///
    pub def fail(_: Input[b]): ParseResult[a, b] =
        ENil

    ///
    /// Returns a parser that succeeds with value `x`
    /// if the input is non-empty and the first
    /// element `x` of the input satisfies
    /// the predicate `p(x)`.
    ///
    /// The parser fails otherwise.
    ///
    pub def satisfy(p: a -> Bool): Parser[a, a] =
        inp -> match inp {
            case ENil                 => fail(inp)
            case ECons(x, xs) if p(x) => succeed(x,       xs)
            case LCons(x, xs) if p(x) => succeed(x, force xs)
            case LList(xs)            => LList(lazy satisfy(p, force xs))
            case _                    => fail(inp)
        }

    ///
    /// Returns a parser that succeeds with value `a`
    /// if the first element of the input is equal to `a`.
    ///
    /// The parser fails otherwise.
    ///
    pub def literal(a: a): Parser[a, a] with Eq[a] =
        satisfy(Eq.eq(a))

    ///
    /// Returns a parser that recognizes the alternation
    /// of `p1` and `p2` (i.e. it recognizes both `p1` and `p2`).
    ///
    /// Equivalent to `p1` | `p2`.
    ///
    pub def otherwise(p1: Parser[a, b], p2: Parser[a, b]): Parser[a, b] =
        inp -> DelayList.append(p1(inp), p2(inp))

    ///
    /// Returns a parser that recognizes the concatenation of
    /// `p1` and `p2`.
    ///
    /// Equivalent to `p1p2`.
    ///
    pub def then(p1: Parser[a, b], p2: Parser[c, b]): Parser[(a, c), b] =
        inp ->
            for (
                (x1, rest1) <- p1(inp);
                (x2, rest2) <- p2(rest1)
            ) yield ((x1, x2), rest2)

    ///
    /// Returns a parser that recognizes the concatenation of
    /// `p1` and `p2` but discards the result of `p1`.
    ///
    pub def thenIgnoringLeft(p1: Parser[a, b], p2: Parser[c, b]): Parser[c, b] =
        (p1 `then` p2) `using` snd

    ///
    /// Returns a parser that recognizes the concatenation of
    /// `p1` and `p2` but discards the result of `p2`.
    ///
    pub def thenIgnoringRight(p1: Parser[a, b], p2: Parser[c, b]): Parser[a, b] =
        (p1 `then` p2) `using` fst

    ///
    /// Returns a parser that applies `f` to all recognized
    /// values of `p`.
    ///
    /// Useful for producing AST nodes.
    ///
    pub def using(p: Parser[a, b], f: a -> c): Parser[c, b] =
        inp ->
            for (
                (x, rest) <- p(inp)
            ) yield (f(x), rest)

    ///
    /// Returns a parser that recognizes zero or more repetitions
    /// of `p`.
    ///
    /// Note that it always succeeds, so the result will always
    /// be non-empty, but the rest of the inp may be empty as well as
    /// the result.
    /// I.e. both the recognized value of may be empty (but still exist) and
    /// the unconsumed may be empty (but still exist).
    ///
    /// Equivalent to `p*`.
    ///
    pub def many(p: Parser[a, b]): Parser[DelayList[a], b] =
        inp -> inp // Wrap in lambda so the recursive call does not immediately happen
            |> (((p `then` many(p)) `using` cons) `otherwise` succeed(ENil))

    ///
    /// Returns a parser that recognizes one or more repetitions
    /// of `p`.
    ///
    /// Note that unlike `many`, this parser may fail, i.e.
    /// not recognize anything.
    ///
    /// Equivalent to `p+`.
    ///
    pub def some(p: Parser[a, b]): Parser[DelayList[a], b] =
        (p `then` many(p)) `using` cons

    ///
    /// Returns a parser that recognizes numbers (consecutive integer characters).
    ///
    /// All possible parses of the number will be recognized.
    ///
    /// E.g. `123` is recognized as `123, 12, 1`.
    ///
    /// The longest match will be the first result.
    ///
    pub def number(inp: Input[Char]): ParseResult[DelayList[Char], Char] =
        let digit = c -> '0' <= c and c <= '9';
        inp |> some(satisfy(digit))

    ///
    /// Returns a parser that recognizes words
    /// (consectutive non-integer, non-whitespace characters).
    ///
    /// All possible parses of the word will be recognized.
    ///
    /// E.g. `hello` is recognized as `hello, hell, hel, he, h`.
    ///
    /// The longest match will be the first result.
    ///
    pub def word(inp: Input[Char]): ParseResult[DelayList[Char], Char] =
        let lowercase = c -> 'a' <= c and c <= 'z';
        let uppercase = c -> 'A' <= c and c <= 'Z';
        let letter = c -> lowercase(c) or uppercase(c);
        inp |> some(satisfy(letter))

    ///
    /// Returns a parser that recognizes the sequence `lit`.
    ///
    /// This is generalization of `literal`.
    ///
    pub def literalSequence(lit: m[a]): Parser[DelayList[a], a] with Eq[a], Foldable[m] =
        inp -> inp // Wrap in lambda so the recursive call does not immediately happen
            |> match Foldable.toList(lit) {
                case Nil     => succeed(ENil)
                case x :: xs => (literal(x) `then` (literalSequence(xs))) `using` cons
            }

    ///
    /// Returns parser that returns the the value `c` if `p` is succesful.
    ///
    pub def return(p: Parser[a, b], c: c): Parser[c, b] =
        let const = (x, _) -> x;
        p `using` const(c)

    ///
    /// Returns a parser that recognizes the string `s`.
    ///
    pub def string(s: String): Parser[DelayList[Char], Char] =
        s |> (String.toList >> literalSequence)

    ///
    /// Returns a parser that ignores whitespace on both
    /// sides of `p`.
    ///
    pub def nibble(p: Parser[a, Char]): Parser[a, Char] =
        whitespace `thenIgnoringLeft` p `thenIgnoringRight` whitespace

    ///
    /// Returns a parser that recognizes whitespace.
    ///
    pub def whitespace(inp: Input[Char]): ParseResult[DelayList[Char], Char] =
        let chars = String.toList(" \t\n");
        inp |> (many(any(literal, chars)))

    ///
    /// Returns a parser that recognizes any of the elements in `syms`.
    ///
    pub def any(f: a -> Parser[b, c], syms: m[a]): Parser[b, c] with Foldable[m] =
        Foldable.foldRight(f >> otherwise, fail, syms)

    ///
    /// Returns the string `s` as an `Input` type.
    ///
    pub def fromString(s: String): Input[Char] =
        String.toList(s) |> List.toDelayList

    ///
    /// Returns `chars` as a string.
    ///
    pub def stringify(chars: m[Char]): String with Foldable[m] = region r {
        let sb = StringBuilder.new(r);
        let ap = sb |> flip(StringBuilder.append!);
        Foldable.forEach(ap, chars);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns the tuple as a list, i.e.
    /// `(x, xs)` is returned as `x :: xs`.
    ///
    def cons(xs: (a, DelayList[a])): DelayList[a] =
        ECons(fst(xs), snd(xs))

}
