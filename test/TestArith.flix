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

namespace TestArith {

    enum Exp with Eq, ToString, Sendable {
        case Num(Int32),
        case Add(Exp, Exp),
        case Sub(Exp, Exp),
        case Mul(Exp, Exp),
        case Div(Exp, Exp)
    }

    def eval(exp0: Exp): Int32 = match exp0 {
        case Num(n)          => n
        case Add(exp1, exp2) => eval(exp1) + eval(exp2)
        case Sub(exp1, exp2) => eval(exp1) - eval(exp2)
        case Mul(exp1, exp2) => eval(exp1) * eval(exp2)
        case Div(exp1, exp2) => eval(exp1) / eval(exp2)
    }

    def show(exp0: Exp): String = match exp0 {
        case Num(n)          => Int32.toString(n)
        case Add(exp1, exp2) => "(${show(exp1)} + ${show(exp2)})"
        case Sub(exp1, exp2) => "(${show(exp1)} - ${show(exp2)})"
        case Mul(exp1, exp2) => "(${show(exp1)} * ${show(exp2)})"
        case Div(exp1, exp2) => "(${show(exp1)} / ${show(exp2)})"
    }

    namespace ArithParser {
        use Parser.{number, literal, using, otherwise, then, thenIgnoringLeft, thenIgnoringRight};
        use TestArith.{Exp => AExp};

        pub def parse(s: String): Option[AExp] =
            Parser.fromString(s) |> expn |> List.head |> Option.map(fst)

        // Each production rule ends with ... `using` func, where func is a function
        // that produces the corresponding AST node.

        def expn(input: List[Char]): ParseResult[AExp, Char] = input |> (
            ((term `then` (literal('+') `thenIgnoringLeft` term)) `using` plus)  `otherwise`
            ((term `then` (literal('-') `thenIgnoringLeft` term)) `using` minus) `otherwise`
            term
        )

        def term(input: List[Char]): ParseResult[AExp, Char] = input |> (
            ((factor `then` (literal('*') `thenIgnoringLeft` factor)) `using` times)  `otherwise`
            ((factor `then` (literal('/') `thenIgnoringLeft` factor)) `using` divide) `otherwise`
            factor
        )

        def factor(input: List[Char]): ParseResult[AExp, Char] = input |> (
            (number `using` value) `otherwise`
            (literal('(') `thenIgnoringLeft` expn `thenIgnoringRight` literal(')'))
        )

        def value(nums: List[Char]): AExp =
            let optInt = Parser.stringify(nums) |> Int32.fromString;
            match optInt {
                case Some(n) => AExp.Num(n)
                case None    => unreachable!()
            }

        def plus(exp: (AExp, AExp)): AExp = match exp {
            case (left, right) => AExp.Add(left, right)
        }

        def minus(exp: (AExp, AExp)): AExp = match exp {
            case (left, right) => AExp.Sub(left, right)
        }

        def times(exp: (AExp, AExp)): AExp = match exp {
            case (left, right) => AExp.Mul(left, right)
        }

        def divide(exp: (AExp, AExp)): AExp = match exp {
            case (left, right) => AExp.Div(left, right)
        }
    }


    ////////////////////////////////////////////////////////////////////////
    // Eval                                                               //
    ////////////////////////////////////////////////////////////////////////

    @test
    def eval01(): Bool =
        let actual = TestArith/ArithParser.parse("");
        let expected = None;
        Assert.eq(expected, actual)

    @test
    def eval02(): Bool =
        let prog = TestArith/ArithParser.parse("1");
        match prog {
            case Some(e) =>
                let actual = eval(e);
                let expected = 1;
                Assert.eq(expected, actual)
            case None => unreachable!()
        }

    @test
    def eval03(): Bool =
        let prog = TestArith/ArithParser.parse("2+2");
        match prog {
            case Some(e) =>
                let actual = eval(e);
                let expected = 4;
                Assert.eq(expected, actual)
            case None => unreachable!()
        }


    ////////////////////////////////////////////////////////////////////////
    // Show                                                               //
    ////////////////////////////////////////////////////////////////////////

    @test
    def show01(): Bool =
        let prog = TestArith/ArithParser.parse("1");
        match prog {
            case Some(e) =>
                let actual = show(e);
                let expected = "1";
                Assert.eq(expected, actual)
            case None => unreachable!()
        }

    @test
    def show02(): Bool =
        let prog = TestArith/ArithParser.parse("2+2");
        match prog {
            case Some(e) =>
                let actual = show(e);
                let expected = "(2 + 2)";
                Assert.eq(expected, actual)
            case None => unreachable!()
        }

}

