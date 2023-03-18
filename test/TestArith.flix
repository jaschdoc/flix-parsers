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

mod TestArith {

    enum Exp with Eq, ToString, Sendable {
        case Num(Int32),
        case Add(Exp, Exp),
        case Sub(Exp, Exp),
        case Mul(Exp, Exp),
        case Div(Exp, Exp)
    }

    def eval(exp0: Exp): Int32 = match exp0 {
        case Exp.Num(n)          => n
        case Exp.Add(exp1, exp2) => eval(exp1) + eval(exp2)
        case Exp.Sub(exp1, exp2) => eval(exp1) - eval(exp2)
        case Exp.Mul(exp1, exp2) => eval(exp1) * eval(exp2)
        case Exp.Div(exp1, exp2) => eval(exp1) / eval(exp2)
    }

    def show(exp0: Exp): String = match exp0 {
        case Exp.Num(n)          => Int32.toString(n)
        case Exp.Add(exp1, exp2) => "(${show(exp1)} + ${show(exp2)})"
        case Exp.Sub(exp1, exp2) => "(${show(exp1)} - ${show(exp2)})"
        case Exp.Mul(exp1, exp2) => "(${show(exp1)} * ${show(exp2)})"
        case Exp.Div(exp1, exp2) => "(${show(exp1)} / ${show(exp2)})"
    }

    mod ArithParser {
        use Parser.{number, literal, using, otherwise, then, thenIgnoringLeft, thenIgnoringRight};
        use TestArith.Exp;

        pub def parse(s: String): Option[Exp] =
            let prog = Parser.fromString(s) |> exp;
            debug(prog |> DelayList.map(match (a, dl) -> (a, DelayList.toList(dl))) |> DelayList.toList);
            prog |> DelayList.head |> Option.map(fst)

        // TODO: Write tests for each parser: factor, term, exp
        // Do this for each parser combined by `otherwise`.

        // Each production rule ends with ... `using` func, where func is a function
        // that produces the corresponding AST node.

        def exp(input: Input[Char]): ParseResult[Exp, Char] = input |> (
            ((term `thenIgnoringRight` literal('+') `then` term) `using` plus)  `otherwise`
            ((term `thenIgnoringRight` literal('-') `then` term) `using` minus) `otherwise`
            term
        )

        def term(input: Input[Char]): ParseResult[Exp, Char] = input |> (
            ((factor `thenIgnoringRight` literal('*') `then` factor) `using` times)  `otherwise`
            ((factor `thenIgnoringRight` literal('/') `then` factor) `using` divide) `otherwise`
            factor
        )

        def factor(input: Input[Char]): ParseResult[Exp, Char] = input |> (
            (number `using` value) `otherwise`
            (literal('(') `thenIgnoringLeft` exp `thenIgnoringRight` literal(')'))
        )

        def value(nums: Input[Char]): Exp =
            let optInt = Parser.stringify(nums) |> Int32.fromString;
            match optInt {
                case Some(n) => Exp.Num(n)
                case None    => unreachable!()
            }

        def plus(exp: (Exp, Exp)): Exp = match exp {
            case (left, right) => Exp.Add(left, right)
        }

        def minus(exp: (Exp, Exp)): Exp = match exp {
            case (left, right) => Exp.Sub(left, right)
        }

        def times(exp: (Exp, Exp)): Exp = match exp {
            case (left, right) => Exp.Mul(left, right)
        }

        def divide(exp: (Exp, Exp)): Exp = match exp {
            case (left, right) => Exp.Div(left, right)
        }
    }


    ////////////////////////////////////////////////////////////////////////
    // Eval                                                               //
    ////////////////////////////////////////////////////////////////////////

    def testEval(expected: Int32, prog: Option[Exp]): Bool = match prog {
        case Some(e) => Assert.eq(expected, eval(e))
        case None => unreachable!()
    }

    // @test
    def _eval01(): Bool =
        let actual = TestArith/ArithParser.parse("");
        let expected = None;
        Assert.eq(expected, actual)

    // @test
    def _eval02(): Bool =
        TestArith/ArithParser.parse("1")
        |> testEval(1)

    // @test
    def _eval03(): Bool =
        TestArith/ArithParser.parse("2+2")
        |> testEval(2 + 2)

    @test
    def _eval04(): Bool =
        TestArith/ArithParser.parse("1+(2/2+3*4*5/5)")
        |> testEval(1+2/2+3*4*5/5)

    // @test
    def _eval05(): Bool =
        TestArith/ArithParser.parse("1/1")
        |> testEval(1)

    // @test
    def _eval06(): Bool =
        TestArith/ArithParser.parse("(2+2)/2+2/(2-1)")
        |> testEval((2 + 2) / 2 + 2 / (2 - 1))


    ////////////////////////////////////////////////////////////////////////
    // Show                                                               //
    ////////////////////////////////////////////////////////////////////////

    def testShow(expected: String, prog: Option[Exp]): Bool = match prog {
        case Some(e) => Assert.eq(expected, show(e))
        case None => unreachable!()
    }

    // @test
    def _show01(): Bool =
        TestArith/ArithParser.parse("1")
        |> testShow("1")

    // @test
    def _show02(): Bool =
        TestArith/ArithParser.parse("2+2")
        |> testShow("(2 + 2)")

    @test
    def _show03(): Bool =
        TestArith/ArithParser.parse("1+(2/3+4*5*6/7)")
        |> testShow("((1 + (2 / 3)) + (((4 * 5) * 6) / 7))")

    // @test
    def _show04(): Bool =
        TestArith/ArithParser.parse("1/1")
        |> testShow("(1 / 1)")

    // @test
    def _show05(): Bool =
        TestArith/ArithParser.parse("(2+2)/2+2/(2-1)")
        |> testShow("(((2 + 2) / 2) + (2 / (2 - 1)))")

}

