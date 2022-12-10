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
    pub def fail(_: Input[b]): ParseResult[a, b] =
        Nil

    ///
    /// Returns a non-empty `ParseResult` if the input is non-empty
    /// and the first element satisfies the predicate `p`.
    /// Returns an empty `ParseResult` otherwise.
    ///
    pub def satisfy(p: a -> Bool \ ef, input: Input[a]): ParseResult[a, a] \ ef = match input {
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

}
