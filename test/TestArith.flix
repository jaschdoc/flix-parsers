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
            prog |> DelayList.head |> Option.map(fst)

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

        @Test
        pub def factor01(): Bool =
            Assert.eq(Some(Exp.Num(1)), factor(Parser.fromString("1")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def factor02(): Bool =
            Assert.eq(Some(Exp.Num(1)), factor(Parser.fromString("(1)")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def factor03(): Bool =
            Assert.eq(Some(Exp.Num(23)), factor(Parser.fromString("(23)")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def term01(): Bool =
            Assert.eq(Some(Exp.Mul(Exp.Num(2), Exp.Num(3))), term(Parser.fromString("2*3")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def term02(): Bool =
            Assert.eq(Some(Exp.Mul(Exp.Num(2), Exp.Num(3))), term(Parser.fromString("(2*3)")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def term03(): Bool =
            Assert.eq(Some(Exp.Div(Exp.Num(2), Exp.Num(3))), term(Parser.fromString("2/3")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def term04(): Bool =
            Assert.eq(Some(Exp.Div(Exp.Num(2), Exp.Num(3))), term(Parser.fromString("(2/3)")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def term05(): Bool =
            Assert.eq(Some(Exp.Num(42)), term(Parser.fromString("42")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def term06(): Bool =
            Assert.eq(Some(Exp.Num(42)), term(Parser.fromString("(42)")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def exp01(): Bool =
            Assert.eq(Some(Exp.Add(Exp.Num(5), Exp.Num(9))), exp(Parser.fromString("5+9")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def exp02(): Bool =
            Assert.eq(Some(Exp.Add(Exp.Num(5), Exp.Num(9))), exp(Parser.fromString("(5+9)")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def exp03(): Bool =
            Assert.eq(Some(Exp.Sub(Exp.Num(5), Exp.Num(9))), exp(Parser.fromString("5-9")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def exp04(): Bool =
            Assert.eq(Some(Exp.Sub(Exp.Num(5), Exp.Num(9))), exp(Parser.fromString("(5-9)")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def exp05(): Bool =
            Assert.eq(Some(Exp.Mul(Exp.Num(42), Exp.Num(2))), exp(Parser.fromString("42*2")) |> DelayList.head |> Option.map(fst))

        @Test
        pub def exp06(): Bool =
            Assert.eq(Some(Exp.Add(Exp.Num(5), Exp.Add(Exp.Num(2), Exp.Num(3)))), exp(Parser.fromString("5+(2+3)")) |> DelayList.head |> Option.map(fst))

    }


    ////////////////////////////////////////////////////////////////////////
    // Eval                                                               //
    ////////////////////////////////////////////////////////////////////////

    def testEval(expected: Int32, prog: Option[Exp]): Bool = match prog {
        case Some(e) => Assert.eq(expected, eval(e))
        case None    => unreachable!()
    }

    @Test
    pub def eval01(): Bool =
        let actual = ArithParser.parse("");
        let expected = None;
        Assert.eq(expected, actual)

    @Test
    pub def eval02(): Bool =
        ArithParser.parse("1")
        |> testEval(1)

    @Test
    pub def eval03(): Bool =
        ArithParser.parse("2+2")
        |> testEval(2 + 2)

    @Test
    pub def eval04(): Bool =
        ArithParser.parse("1+(2/2+3*(4*(5/5)))")
        |> testEval(1+2/2+3*4*5/5)

    @Test
    pub def eval05(): Bool =
        ArithParser.parse("1/1")
        |> testEval(1)

    @Test
    pub def eval06(): Bool =
        ArithParser.parse("(2+2)/2+2/(2-1)")
        |> testEval((2 + 2) / 2 + 2 / (2 - 1))


    ////////////////////////////////////////////////////////////////////////
    // Show                                                               //
    ////////////////////////////////////////////////////////////////////////

    def testShow(expected: String, prog: Option[Exp]): Bool = match prog {
        case Some(e) => Assert.eq(expected, show(e))
        case None    => unreachable!()
    }

    @Test
    pub def show01(): Bool =
        ArithParser.parse("1")
        |> testShow("1")

    @Test
    pub def show02(): Bool =
        ArithParser.parse("2+2")
        |> testShow("(2 + 2)")

    @Test
    pub def show03(): Bool =
        ArithParser.parse("1+(2/3+4*(5*(6/7)))")
        |> testShow("(1 + ((2 / 3) + (4 * (5 * (6 / 7)))))")

    @Test
    pub def show04(): Bool =
        ArithParser.parse("1/1")
        |> testShow("(1 / 1)")

    @Test
    pub def show05(): Bool =
        ArithParser.parse("(2+2)/2+2/(2-1)")
        |> testShow("(((2 + 2) / 2) + (2 / (2 - 1)))")

}

