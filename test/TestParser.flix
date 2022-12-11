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

    use Parser.{succeed, fail, literal, otherwise,
                then, thenIgnoringLeft, nibble,
                thenIgnoringRight, using, many,
                some, number, word, input, stringify,
                literalSequence, string, return};

    // Helper functions

    def noInput(): Input[Char] = Nil

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
    // Otherwise                                                          //
    ////////////////////////////////////////////////////////////////////////

    @test
    def otherwise01(): Bool =
        let input = noInput();
        let actual = otherwise(literal('a'), literal('b'), input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise02(): Bool =
        let input = input("a");
        let actual = otherwise(literal('a'), literal('b'), input);
        let expected = ('a', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise03(): Bool =
        let input = "a" :: "b" :: Nil;
        let actual = otherwise(literal("a"), literal("b"), input);
        let expected = ("a", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise04(): Bool =
        let input = "a" :: "b" :: Nil;
        let actual = otherwise(literal("a"), literal("a"), input);
        let expected = ("a", "b" :: Nil) :: ("a", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise05(): Bool =
        let input = "b" :: "a" :: Nil;
        let actual = otherwise(literal("a"), literal("b"), input);
        let expected = ("b", "a" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise06(): Bool =
        let input = "b" :: "b" :: Nil;
        let actual = otherwise(literal("a"), literal("b"), input);
        let expected = ("b", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise07(): Bool =
        let input = "b" :: "c" :: Nil;
        let actual = otherwise(literal("b"), literal("b"), input);
        let expected = ("b", "c" :: Nil) :: ("b", "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise08(): Bool =
        let input = input("c");
        let actual = otherwise(literal('a'), literal('b'), input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise09(): Bool =
        let input = noInput();
        let actual = otherwise(succeed("1"), succeed("2"), input);
        let expected = ("1", Nil) :: ("2", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise10(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = otherwise(succeed("1"), succeed("2"), input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: ("2", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise11(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = otherwise(succeed("1"), fail, input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise12(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = otherwise(fail, succeed("1"), input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise13(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual: ParseResult[String, String] = otherwise(fail, fail, input);
        let expected = Nil;
        Assert.eq(expected, actual)

    // Associativity

    @test
    def otherwise14(): Bool =
        let input = noInput();
        let p1 = otherwise(succeed("a"), succeed("b"));
        let actual = otherwise(p1, succeed("c"), input);
        let expected = ("a", Nil) :: ("b", Nil) :: ("c", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def otherwise15(): Bool =
        let input = noInput();
        let p2 = otherwise(succeed("b"), succeed("c"));
        let actual = otherwise(succeed("a"), p2, input);
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
    // ThenIgnoringLeft                                                   //
    ////////////////////////////////////////////////////////////////////////

    @test
    def thenIgnoringLeft01(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (fail `thenIgnoringLeft` fail);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft02(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (fail `thenIgnoringLeft` succeed('a'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft03(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (succeed('a') `thenIgnoringLeft` fail);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft04(): Bool =
        let input = noInput();
        let actual = input |> (succeed('a') `thenIgnoringLeft` succeed('b'));
        let expected = ('b', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft05(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` literal('b'));
        let expected = ('b', 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft06(): Bool =
        let input = input("abc");
        let actual = input |> (literal('b') `thenIgnoringLeft` literal('a'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft07(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` literal('c'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft08(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` literal('b') `thenIgnoringLeft` literal('c'));
        let expected = ('c', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft09(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` (literal('b') `thenIgnoringLeft` literal('c')));
        let expected = ('c', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft10(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `then` literal('b') `thenIgnoringLeft` literal('c'));
        let expected = ('c', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft11(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `then` (literal('b') `thenIgnoringLeft` literal('c')));
        let expected = (('a', 'c'), Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft12(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` literal('b') `then` literal('c'));
        let expected = (('b', 'c'), Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringLeft13(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` (literal('b') `then` literal('c')));
        let expected = (('b', 'c'), Nil) :: Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // ThenIgnoringRight                                                  //
    ////////////////////////////////////////////////////////////////////////

    @test
    def thenIgnoringRight01(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (fail `thenIgnoringRight` fail);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight02(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (fail `thenIgnoringRight` succeed('a'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight03(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (succeed('a') `thenIgnoringRight` fail);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight04(): Bool =
        let input = noInput();
        let actual = input |> (succeed('a') `thenIgnoringRight` succeed('b'));
        let expected = ('a', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight05(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` literal('b'));
        let expected = ('a', 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight06(): Bool =
        let input = input("abc");
        let actual = input |> (literal('b') `thenIgnoringRight` literal('a'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight07(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` literal('c'));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight08(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` literal('b') `thenIgnoringRight` literal('c'));
        let expected = ('a', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight09(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` (literal('b') `thenIgnoringRight` literal('c')));
        let expected = ('a', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight10(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `then` literal('b') `thenIgnoringRight` literal('c'));
        let expected = (('a', 'b'), Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight11(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `then` (literal('b') `thenIgnoringRight` literal('c')));
        let expected = (('a', 'b'), Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight12(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` literal('b') `then` literal('c'));
        let expected = (('a', 'c'), Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def thenIgnoringRight13(): Bool =
        let input = input("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` (literal('b') `then` literal('c')));
        let expected = ('a', Nil) :: Nil;
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


    ////////////////////////////////////////////////////////////////////////
    // LiteralSequence                                                    //
    ////////////////////////////////////////////////////////////////////////

    @test
    def literalSequence01(): Bool =
        let input = noInput();
        let actual = literalSequence(Nil, input);
        let expected = (Nil, Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def literalSequence02(): Bool =
        let input = input("abc");
        let actual = literalSequence(Nil, input);
        let expected = (Nil, 'a' :: 'b' :: 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def literalSequence03(): Bool =
        let input = input("abc");
        let actual = literalSequence('a' :: 'b' :: Nil, input);
        let expected = ('a' :: 'b' :: Nil, 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def literalSequence04(): Bool =
        let input = input("abc");
        let actual = literalSequence('a' :: 'b' :: 'c' :: Nil, input);
        let expected = ('a' :: 'b' :: 'c' :: Nil, Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def literalSequence05(): Bool =
        let input = input("abc");
        let actual = literalSequence('b' :: 'a' :: Nil, input);
        let expected = Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // String                                                             //
    ////////////////////////////////////////////////////////////////////////

    @test
    def string01(): Bool =
        let input = noInput();
        let actual = string("", input);
        let expected = (Nil, Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def string02(): Bool =
        let input = input("abc");
        let actual = string("", input);
        let expected = (Nil, 'a' :: 'b' :: 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def string03(): Bool =
        let input = input("abc");
        let actual = string("ab", input);
        let expected = ('a' :: 'b' :: Nil, 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def string04(): Bool =
        let input = input("abc");
        let actual = string("abc", input);
        let expected = ('a' :: 'b' :: 'c' :: Nil, Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def string05(): Bool =
        let input = input("abc");
        let actual = string("ba", input);
        let expected = Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Return                                                             //
    ////////////////////////////////////////////////////////////////////////

    @test
    def return01(): Bool =
        let input = noInput();
        let actual = input |> (succeed(Nil) `return` "");
        let expected = ("", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def return02(): Bool =
        let input = noInput();
        let actual = input |> (fail `return` "");
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def return03(): Bool =
        let input = input("");
        let actual = input |> (string("") `return` "");
        let expected = ("", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def return04(): Bool =
        let input = input("");
        let actual = input |> (string("") `return` 'a');
        let expected = ('a', Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def return05(): Bool =
        let input = input("");
        let actual = input |> (string("") `return` 1);
        let expected = (1, Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def return06(): Bool =
        let input = input("abc");
        let actual = input |> (string("") `return` 1);
        let expected = (1, 'a' :: 'b' :: 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def return07(): Bool =
        let input = input("abc");
        let actual = input |> (string("ab") `return` 1);
        let expected = (1, 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def return08(): Bool =
        let input = input("abc");
        let actual = input |> (string("ab") `return` "");
        let expected = ("", 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Stringify                                                          //
    ////////////////////////////////////////////////////////////////////////

    @test
    def stringify01(): Bool =
        let input = input("");
        let actual = stringify(input);
        let expected = "";
        Assert.eq(expected, actual)

    @test
    def stringify02(): Bool =
        let input = input("aaab");
        let actual = stringify(input);
        let expected = "aaab";
        Assert.eq(expected, actual)

    @test
    def stringify03(): Bool =
        let input = input("12345");
        let actual = number(input)
            |> List.head
            |> Option.map(fst >> stringify);
        let expected = Some("12345");
        Assert.eq(expected, actual)

    ////////////////////////////////////////////////////////////////////////
    // Nibble                                                             //
    ////////////////////////////////////////////////////////////////////////

    @test
    def nibble01(): Bool =
        let input = input("");
        let actual: ParseResult[Char, Char] = input |> (nibble(fail));
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def nibble02(): Bool =
        let input = input("");
        let actual = input |> (nibble(succeed("")));
        let expected = ("", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def nibble03(): Bool =
        let input = input("");                          // `succeed` yields whatever
        let actual = input |> (nibble(succeed("   "))); // it's passed regardless of
        let expected = ("   ", Nil) :: Nil;             // any function that processes the input
        Assert.eq(expected, actual)

    @test
    def nibble04(): Bool =
        let input = input("");                             // `succeed` yields whatever
        let actual = input |> (nibble(succeed("  abc "))); // it's passed regardless of
        let expected = ("  abc ", Nil) :: Nil;             // any function that processes the input
        Assert.eq(expected, actual)

    @test
    def nibble05(): Bool =
        let input = input("    aaa      ");
        let actual = input |> (nibble(string("aaa")));
        let expected = ('a' :: 'a' :: 'a' :: Nil, Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def nibble06(): Bool =
        let input = input("    aaaabc");
        let actual = input |> (nibble(string("aaa")));
        let expected = ('a' :: 'a' :: 'a' :: Nil, 'a' :: 'b' :: 'c' :: Nil) :: Nil;
        Assert.eq(expected, actual)

}
