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

namespace TestParser {

    use Parser.{succeed, fail, literal, either,
                then, using, many, some, number,
                word};

    // Helper functions

    def noInput(): Input[Char] = Nil

    def input(s: String): Input[Char] =
        Parser.inputFromString(s)

    ////////////////////////////////////////////////////////////////////////
    // Succeed                                                            //
    ////////////////////////////////////////////////////////////////////////

    @test
    def succeed01(): Bool =
        let input = noInput();
        let actual = succeed("", input);
        let expected = ("", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def succeed02(): Bool =
        let input = noInput();
        let actual = succeed("a", input);
        let expected = ("a", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def succeed03(): Bool =
        let input = input("abc");
        let actual = succeed("a", input);
        let expected = ("a", input) :: Nil;
        Assert.eq(expected, actual)

    @test
    def succeed04(): Bool =
        let input = "abc" :: "123" :: "hey" :: Nil;
        let actual = succeed("success", input);
        let expected = ("success", input) :: Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Fail                                                               //
    ////////////////////////////////////////////////////////////////////////

    @test
    def fail01(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = fail(input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def fail03(): Bool =
        let input = input("abc");
        let actual: ParseResult[String, Char] = fail(input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def fail04(): Bool =
        let input = "abc" :: "123" :: "hey" :: Nil;
        let actual: ParseResult[String, String] = fail(input);
        let expected = Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Satisfy                                                            //
    ////////////////////////////////////////////////////////////////////////

    @test
    def satisfy01(): Bool =
        let input = noInput();
        let actual = Parser.satisfy(a -> a == 'a', input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def satisfy02(): Bool =
        let input = input("a");
        let actual = Parser.satisfy(a -> a == 'a', input);
        let expected = ('a', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def satisfy03(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = Parser.satisfy(a -> a == "a", input);
        let expected = ("a", "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def satisfy04(): Bool =
        let input = "b" :: "a" :: "c" :: Nil;
        let actual = Parser.satisfy(a -> a == "a", input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def satisfy05(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = Parser.satisfy(a -> a == "b", input);
        let expected = Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Literal                                                            //
    ////////////////////////////////////////////////////////////////////////

    @test
    def literal01(): Bool =
        let input = noInput();
        let actual = literal('a', input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def literal02(): Bool =
        let input = input("a");
        let actual = literal('a', input);
        let expected = ('a', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def literal03(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = literal("a", input);
        let expected = ("a", "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def literal04(): Bool =
        let input = "b" :: "a" :: "c" :: Nil;
        let actual = literal("a", input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def literal05(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = literal("b", input);
        let expected = Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // either                                                             //
    ////////////////////////////////////////////////////////////////////////

    @test
    def either01(): Bool =
        let input = noInput();
        let actual = either(literal('a'), literal('b'), input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def either02(): Bool =
        let input = input("a");
        let actual = either(literal('a'), literal('b'), input);
        let expected = ('a', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either03(): Bool =
        let input = "a" :: "b" :: Nil;
        let actual = either(literal("a"), literal("b"), input);
        let expected = ("a", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either04(): Bool =
        let input = "a" :: "b" :: Nil;
        let actual = either(literal("a"), literal("a"), input);
        let expected = ("a", "b" :: Nil) :: ("a", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either05(): Bool =
        let input = "b" :: "a" :: Nil;
        let actual = either(literal("a"), literal("b"), input);
        let expected = ("b", "a" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either06(): Bool =
        let input = "b" :: "b" :: Nil;
        let actual = either(literal("a"), literal("b"), input);
        let expected = ("b", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either07(): Bool =
        let input = "b" :: "c" :: Nil;
        let actual = either(literal("b"), literal("b"), input);
        let expected = ("b", "c" :: Nil) :: ("b", "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either08(): Bool =
        let input = input("c");
        let actual = either(literal('a'), literal('b'), input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def either09(): Bool =
        let input = noInput();
        let actual = either(succeed("1"), succeed("2"), input);
        let expected = ("1", Nil) :: ("2", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either10(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = either(succeed("1"), succeed("2"), input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: ("2", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either11(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = either(succeed("1"), fail, input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either12(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = either(fail, succeed("1"), input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either13(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual: ParseResult[String, String] = either(fail, fail, input);
        let expected = Nil;
        Assert.eq(expected, actual)

    // Associativity

    @test
    def either14(): Bool =
        let input = noInput();
        let p1 = either(succeed("a"), succeed("b"));
        let actual = either(p1, succeed("c"), input);
        let expected = ("a", Nil) :: ("b", Nil) :: ("c", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def either15(): Bool =
        let input = noInput();
        let p2 = either(succeed("b"), succeed("c"));
        let actual = either(succeed("a"), p2, input);
        let expected = ("a", Nil) :: ("b", Nil) :: ("c", Nil) :: Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Then                                                               //
    ////////////////////////////////////////////////////////////////////////

    @test
    def then01(): Bool =
        let input = noInput();
        let actual: ParseResult[(Char, Char), Char] = input |> (fail `then` fail);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def then02(): Bool =
        let input = noInput();
        let actual: ParseResult[(Char, Char), Char] = input |> (fail `then` succeed('a'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def then03(): Bool =
        let input = noInput();
        let actual: ParseResult[(Char, Char), Char] = input |> (succeed('a') `then` fail);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def then04(): Bool =
        let input = noInput();
        let actual = input |> (succeed('a') `then` succeed('b'));
        let expected = (('a', 'b'), Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def then05(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `then` literal('b'));
        let expected = (('a', 'b'), 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def then06(): Bool =
        let input = input("abc");
        let actual = input |> (literal('b') `then` literal('a'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def then07(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `then` literal('c'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def then08(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `then` literal('b') `then` literal('c'));
        let expected = ((('a', 'b'), 'c'), Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def then09(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `then` (literal('b') `then` literal('c')));
        let expected = (('a', ('b', 'c')), Nil) :: Nil;
        Assert.eq(expected, actual)

    ////////////////////////////////////////////////////////////////////////
    // Using                                                              //
    ////////////////////////////////////////////////////////////////////////

    @test
    def using01(): Bool =
        let input = noInput();
        let actual: ParseResult[String, Char] = input |> (fail `using` (c -> if (c == 'c') "C" else ""));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def using02(): Bool =
        let input = noInput();
        let actual = input |> (succeed(1) `using` (x -> x + 2));
        let expected = (3, Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def using03(): Bool =
        let input = input("abc");
        let f = c -> if (c == 'c') 1 else 0;
        let actual = input |> (literal('a') `using` f);
        let expected = (0, 'b' :: 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def using04(): Bool =
        let input = input("cba");
        let f = c -> if (c == 'c') 1 else 0;
        let actual = input |> (literal('c') `using` f);
        let expected = (1, 'b' :: 'a' :: Nil) :: Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Many                                                               //
    ////////////////////////////////////////////////////////////////////////

    @test
    def many01(): Bool =
        let input = noInput();
        let actual = input |> many(literal('a'));
        let expected = (Nil, Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def many02(): Bool =
        let input = input("b");
        let actual = input |> many(literal('a'));
        let expected = (Nil, 'b' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def many03(): Bool =
        let input = input("ab");
        let actual = input |> many(literal('a'));
        let expected = ('a' :: Nil, 'b' :: Nil) :: (Nil, 'a' :: 'b' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def many04(): Bool =
        let input = input("aaab");
        let actual = input |> many(literal('a'));
        let expected =
            ('a' :: 'a' :: 'a' :: Nil, 'b' :: Nil) ::
            ('a' :: 'a' :: Nil, 'a' :: 'b' :: Nil) ::
            ('a' :: Nil, 'a' :: 'a' :: 'b' :: Nil) ::
            (Nil, 'a' :: 'a' :: 'a' :: 'b' :: Nil) ::
            Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Some                                                               //
    ////////////////////////////////////////////////////////////////////////

    @test
    def some01(): Bool =
        let input = noInput();
        let actual = input |> some(literal('a'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def some02(): Bool =
        let input = input("b");
        let actual = input |> some(literal('a'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def some03(): Bool =
        let input = input("ab");
        let actual = input |> some(literal('a'));
        let expected = ('a' :: Nil, 'b' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def some04(): Bool =
        let input = input("aaab");
        let actual = input |> some(literal('a'));
        let expected =
            ('a' :: 'a' :: 'a' :: Nil, 'b' :: Nil) ::
            ('a' :: 'a' :: Nil, 'a' :: 'b' :: Nil) ::
            ('a' :: Nil, 'a' :: 'a' :: 'b' :: Nil) ::
            Nil;
        Assert.eq(expected, actual)

    @test
    def some05(): Bool =
        let input = input("baaa");
        let actual = input |> some(literal('a'));
        let expected = Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Number                                                             //
    ////////////////////////////////////////////////////////////////////////

    @test
    def number01(): Bool =
        let input = noInput();
        let actual = number(input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def number02(): Bool =
        let input = input("abc");
        let actual = number(input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def number03(): Bool =
        let input = input("1 big apple");
        let rest = String.toList(" big apple");
        let actual = number(input);
        let expected = ('1' :: Nil, rest) :: Nil;
        Assert.eq(expected, actual)

    @test
    def number04(): Bool =
        let input = input("123 big apple");
        let rest = String.toList(" big apple");
        let actual = number(input);
        let expected =
            ('1' :: '2' :: '3' :: Nil, rest) ::
            ('1' :: '2' :: Nil, '3' :: rest) ::
            ('1' :: Nil, '2' :: '3' :: rest) ::
            Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Word                                                               //
    ////////////////////////////////////////////////////////////////////////

    @test
    def word01(): Bool =
        let input = noInput();
        let actual = word(input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def word02(): Bool =
        let input = input("abc");
        let actual = word(input);
        let expected =
            ('a' :: 'b' :: 'c' :: Nil, Nil) ::
            ('a' :: 'b' :: Nil, 'c' :: Nil) ::
            ('a' :: Nil, 'b' :: 'c' :: Nil) ::
            Nil;
        Assert.eq(expected, actual)

    @test
    def word03(): Bool =
        let input = input("1 big apple");
        let actual = word(input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def word04(): Bool =
        let input = input("big 123 apple");
        let rest = String.toList(" 123 apple");
        let actual = word(input);
        let expected =
            ('b' :: 'i' :: 'g' :: Nil, rest) ::
            ('b' :: 'i' :: Nil, 'g' :: rest) ::
            ('b' :: Nil, 'i' :: 'g' :: rest) ::
            Nil;
        Assert.eq(expected, actual)

}
