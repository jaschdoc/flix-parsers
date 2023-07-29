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

mod TestParser {

    use DelayList.ENil;

    use Parser.{succeed, fail, literal, otherwise,
                then, thenIgnoringLeft, nibble,
                thenIgnoringRight, using, many,
                some, number, word, fromString,
                literalSequence, string, return,
                stringify};

    // Helper functions

    def noInput(): Input[Char] = ENil

    def l2d(l: m[a]): DelayList[a] with Foldable[m] = Foldable.toDelayList(l)

    def d2l(l: DelayList[a]): List[a] = DelayList.toList(l)

    def pr(l: List[(a, List[b])]): ParseResult[a, b] =
        l |> List.map(match (a, ll) -> (a, l2d(ll)))
          |> l2d

    def pr2(l: List[(List[a], List[b])]): ParseResult[DelayList[a], b] =
        l |> List.map(match (a, ll) -> (l2d(a), l2d(ll)))
          |> l2d

    ////////////////////////////////////////////////////////////////////////
    // Succeed                                                            //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def succeed01(): Bool =
        let input = noInput();
        let actual = succeed("", input);
        let expected = pr(("", Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def succeed02(): Bool =
        let input = noInput();
        let actual = succeed("a", input);
        let expected = pr(("a", Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def succeed03(): Bool =
        let input = fromString("abc");
        let actual = succeed("a", input);
        let expected = pr(("a", d2l(input)) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def succeed04(): Bool =
        let input = "abc" :: "123" :: "hey" :: Nil;
        let actual = succeed("success", l2d(input));
        let expected = pr(("success", input) :: Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Fail                                                               //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def fail01(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = fail(input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def fail03(): Bool =
        let input = fromString("abc");
        let actual: ParseResult[String, Char] = fail(input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def fail04(): Bool =
        let input = l2d("abc" :: "123" :: "hey" :: Nil);
        let actual: ParseResult[String, String] = fail(input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Satisfy                                                            //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def satisfy01(): Bool =
        let input = noInput();
        let actual = Parser.satisfy(a -> a == 'a', input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def satisfy02(): Bool =
        let input = fromString("a");
        let actual = Parser.satisfy(a -> a == 'a', input);
        let expected = pr(('a', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def satisfy03(): Bool =
        let input = l2d("a" :: "b" :: "c" :: Nil);
        let actual = Parser.satisfy(a -> a == "a", input);
        let expected = pr(("a", "b" :: "c" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def satisfy04(): Bool =
        let input = l2d("b" :: "a" :: "c" :: Nil);
        let actual = Parser.satisfy(a -> a == "a", input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def satisfy05(): Bool =
        let input = l2d("a" :: "b" :: "c" :: Nil);
        let actual = Parser.satisfy(a -> a == "b", input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Literal                                                            //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def literal01(): Bool =
        let input = noInput();
        let actual = literal('a', input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def literal02(): Bool =
        let input = fromString("a");
        let actual = literal('a', input);
        let expected = pr(('a', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def literal03(): Bool =
        let input = l2d("a" :: "b" :: "c" :: Nil);
        let actual = literal("a", input);
        let expected = pr(("a", "b" :: "c" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def literal04(): Bool =
        let input = l2d("b" :: "a" :: "c" :: Nil);
        let actual = literal("a", input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def literal05(): Bool =
        let input = l2d("a" :: "b" :: "c" :: Nil);
        let actual = literal("b", input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Otherwise                                                          //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def otherwise01(): Bool =
        let input = noInput();
        let actual = otherwise(literal('a'), literal('b'), input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise02(): Bool =
        let input = fromString("a");
        let actual = otherwise(literal('a'), literal('b'), input);
        let expected = pr(('a', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise03(): Bool =
        let input = l2d("a" :: "b" :: Nil);
        let actual = otherwise(literal("a"), literal("b"), input);
        let expected = pr(("a", "b" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise04(): Bool =
        let input = l2d("a" :: "b" :: Nil);
        let actual = otherwise(literal("a"), literal("a"), input);
        let expected = pr(("a", "b" :: Nil) :: ("a", "b" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise05(): Bool =
        let input = l2d("b" :: "a" :: Nil);
        let actual = otherwise(literal("a"), literal("b"), input);
        let expected = pr(("b", "a" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise06(): Bool =
        let input = l2d("b" :: "b" :: Nil);
        let actual = otherwise(literal("a"), literal("b"), input);
        let expected = pr(("b", "b" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise07(): Bool =
        let input = l2d("b" :: "c" :: Nil);
        let actual = otherwise(literal("b"), literal("b"), input);
        let expected = pr(("b", "c" :: Nil) :: ("b", "c" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise08(): Bool =
        let input = fromString("c");
        let actual = otherwise(literal('a'), literal('b'), input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise09(): Bool =
        let input = noInput();
        let actual = otherwise(succeed("1"), succeed("2"), input);
        let expected = pr(("1", Nil) :: ("2", Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise10(): Bool =
        let input = l2d("a" :: "b" :: "c" :: Nil);
        let actual = otherwise(succeed("1"), succeed("2"), input);
        let expected = pr(("1", "a" :: "b" :: "c" :: Nil) :: ("2", "a" :: "b" :: "c" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise11(): Bool =
        let input = l2d("a" :: "b" :: "c" :: Nil);
        let actual = otherwise(succeed("1"), fail, input);
        let expected = pr(("1", "a" :: "b" :: "c" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise12(): Bool =
        let input = l2d("a" :: "b" :: "c" :: Nil);
        let actual = otherwise(fail, succeed("1"), input);
        let expected = pr(("1", "a" :: "b" :: "c" :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise13(): Bool =
        let input = l2d("a" :: "b" :: "c" :: Nil);
        let actual: ParseResult[String, String] = otherwise(fail, fail, input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    // Associativity

    @Test
    def otherwise14(): Bool =
        let input = noInput();
        let p1 = otherwise(succeed("a"), succeed("b"));
        let actual = otherwise(p1, succeed("c"), input);
        let expected = pr(("a", Nil) :: ("b", Nil) :: ("c", Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def otherwise15(): Bool =
        let input = noInput();
        let p2 = otherwise(succeed("b"), succeed("c"));
        let actual = otherwise(succeed("a"), p2, input);
        let expected = pr(("a", Nil) :: ("b", Nil) :: ("c", Nil) :: Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Then                                                               //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def then01(): Bool =
        let input = noInput();
        let actual: ParseResult[(Char, Char), Char] = input |> (fail `then` fail);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def then02(): Bool =
        let input = noInput();
        let actual: ParseResult[(Char, Char), Char] = input |> (fail `then` succeed('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def then03(): Bool =
        let input = noInput();
        let actual: ParseResult[(Char, Char), Char] = input |> (succeed('a') `then` fail);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def then04(): Bool =
        let input = noInput();
        let actual = input |> (succeed('a') `then` succeed('b'));
        let expected = pr((('a', 'b'), Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def then05(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `then` literal('b'));
        let expected = pr((('a', 'b'), 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def then06(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('b') `then` literal('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def then07(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `then` literal('c'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def then08(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `then` literal('b') `then` literal('c'));
        let expected = pr(((('a', 'b'), 'c'), Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def then09(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `then` (literal('b') `then` literal('c')));
        let expected = pr((('a', ('b', 'c')), Nil) :: Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // ThenIgnoringLeft                                                   //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def thenIgnoringLeft01(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (fail `thenIgnoringLeft` fail);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft02(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (fail `thenIgnoringLeft` succeed('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft03(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (succeed('a') `thenIgnoringLeft` fail);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft04(): Bool =
        let input = noInput();
        let actual = input |> (succeed('a') `thenIgnoringLeft` succeed('b'));
        let expected = pr(('b', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft05(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` literal('b'));
        let expected = pr(('b', 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft06(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('b') `thenIgnoringLeft` literal('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft07(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` literal('c'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft08(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` literal('b') `thenIgnoringLeft` literal('c'));
        let expected = pr(('c', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft09(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` (literal('b') `thenIgnoringLeft` literal('c')));
        let expected = pr(('c', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft10(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `then` literal('b') `thenIgnoringLeft` literal('c'));
        let expected = pr(('c', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft11(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `then` (literal('b') `thenIgnoringLeft` literal('c')));
        let expected = pr((('a', 'c'), Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft12(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` literal('b') `then` literal('c'));
        let expected = pr((('b', 'c'), Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringLeft13(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringLeft` (literal('b') `then` literal('c')));
        let expected = pr((('b', 'c'), Nil) :: Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // ThenIgnoringRight                                                  //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def thenIgnoringRight01(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (fail `thenIgnoringRight` fail);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight02(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (fail `thenIgnoringRight` succeed('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight03(): Bool =
        let input = noInput();
        let actual: ParseResult[Char, Char] = input |> (succeed('a') `thenIgnoringRight` fail);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight04(): Bool =
        let input = noInput();
        let actual = input |> (succeed('a') `thenIgnoringRight` succeed('b'));
        let expected = pr(('a', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight05(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` literal('b'));
        let expected = pr(('a', 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight06(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('b') `thenIgnoringRight` literal('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight07(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` literal('c'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight08(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` literal('b') `thenIgnoringRight` literal('c'));
        let expected = pr(('a', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight09(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` (literal('b') `thenIgnoringRight` literal('c')));
        let expected = pr(('a', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight10(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `then` literal('b') `thenIgnoringRight` literal('c'));
        let expected = pr((('a', 'b'), Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight11(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `then` (literal('b') `thenIgnoringRight` literal('c')));
        let expected = pr((('a', 'b'), Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight12(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` literal('b') `then` literal('c'));
        let expected = pr((('a', 'c'), Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def thenIgnoringRight13(): Bool =
        let input = fromString("abc");
        let actual = input |> (literal('a') `thenIgnoringRight` (literal('b') `then` literal('c')));
        let expected = pr(('a', Nil) :: Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Using                                                              //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def using01(): Bool =
        let input = noInput();
        let actual: ParseResult[String, Char] = input |> (fail `using` (c -> if (c == 'c') "C" else ""));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def using02(): Bool =
        let input = noInput();
        let actual = input |> (succeed(1) `using` (x -> x + 2));
        let expected = pr((3, Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def using03(): Bool =
        let input = fromString("abc");
        let f = c -> if (c == 'c') 1 else 0;
        let actual = input |> (literal('a') `using` f);
        let expected = pr((0, 'b' :: 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def using04(): Bool =
        let input = fromString("cba");
        let f = c -> if (c == 'c') 1 else 0;
        let actual = input |> (literal('c') `using` f);
        let expected = pr((1, 'b' :: 'a' :: Nil) :: Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Many                                                               //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def many01(): Bool =
        let input = noInput();
        let actual = input |> many(literal('a'));
        let expected = pr2((Nil, Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def many02(): Bool =
        let input = fromString("b");
        let actual = input |> many(literal('a'));
        let expected = pr2((Nil, 'b' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def many03(): Bool =
        let input = fromString("ab");
        let actual = input |> many(literal('a'));
        let expected = pr2(('a' :: Nil, 'b' :: Nil) :: (Nil, 'a' :: 'b' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def many04(): Bool =
        let input = fromString("aaab");
        let actual = input |> many(literal('a'));
        let expected = pr2(
            ('a' :: 'a' :: 'a' :: Nil, 'b' :: Nil) ::
            ('a' :: 'a' :: Nil, 'a' :: 'b' :: Nil) ::
            ('a' :: Nil, 'a' :: 'a' :: 'b' :: Nil) ::
            (Nil, 'a' :: 'a' :: 'a' :: 'b' :: Nil) ::
            Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Some                                                               //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def some01(): Bool =
        let input = noInput();
        let actual = input |> some(literal('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def some02(): Bool =
        let input = fromString("b");
        let actual = input |> some(literal('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def some03(): Bool =
        let input = fromString("ab");
        let actual = input |> some(literal('a'));
        let expected = pr2(('a' :: Nil, 'b' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def some04(): Bool =
        let input = fromString("aaab");
        let actual = input |> some(literal('a'));
        let expected = pr2(
            ('a' :: 'a' :: 'a' :: Nil, 'b' :: Nil) ::
            ('a' :: 'a' :: Nil, 'a' :: 'b' :: Nil) ::
            ('a' :: Nil, 'a' :: 'a' :: 'b' :: Nil) ::
            Nil);
        Assert.eq(expected, actual)

    @Test
    def some05(): Bool =
        let input = fromString("baaa");
        let actual = input |> some(literal('a'));
        let expected = pr(Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Number                                                             //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def number01(): Bool =
        let input = noInput();
        let actual = number(input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def number02(): Bool =
        let input = fromString("abc");
        let actual = number(input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def number03(): Bool =
        let input = fromString("1 big apple");
        let rest = String.toList(" big apple");
        let actual = number(input);
        let expected = pr2(('1' :: Nil, rest) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def number04(): Bool =
        let input = fromString("123 big apple");
        let rest = String.toList(" big apple");
        let actual = number(input);
        let expected = pr2(
            ('1' :: '2' :: '3' :: Nil, rest) ::
            ('1' :: '2' :: Nil, '3' :: rest) ::
            ('1' :: Nil, '2' :: '3' :: rest) ::
            Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Word                                                               //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def word01(): Bool =
        let input = noInput();
        let actual = word(input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def word02(): Bool =
        let input = fromString("abc");
        let actual = word(input);
        let expected = pr2(
            ('a' :: 'b' :: 'c' :: Nil, Nil) ::
            ('a' :: 'b' :: Nil, 'c' :: Nil) ::
            ('a' :: Nil, 'b' :: 'c' :: Nil) ::
            Nil);
        Assert.eq(expected, actual)

    @Test
    def word03(): Bool =
        let input = fromString("1 big apple");
        let actual = word(input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def word04(): Bool =
        let input = fromString("big 123 apple");
        let rest = String.toList(" 123 apple");
        let actual = word(input);
        let expected = pr2(
            ('b' :: 'i' :: 'g' :: Nil, rest) ::
            ('b' :: 'i' :: Nil, 'g' :: rest) ::
            ('b' :: Nil, 'i' :: 'g' :: rest) ::
            Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // LiteralSequence                                                    //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def literalSequence01(): Bool =
        let input = noInput();
        let actual = literalSequence(Nil, input);
        let expected = pr2((Nil, Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def literalSequence02(): Bool =
        let input = fromString("abc");
        let actual = literalSequence(Nil, input);
        let expected = pr2((Nil, 'a' :: 'b' :: 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def literalSequence03(): Bool =
        let input = fromString("abc");
        let actual = literalSequence('a' :: 'b' :: Nil, input);
        let expected = pr2(('a' :: 'b' :: Nil, 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def literalSequence04(): Bool =
        let input = fromString("abc");
        let actual = literalSequence('a' :: 'b' :: 'c' :: Nil, input);
        let expected = pr2(('a' :: 'b' :: 'c' :: Nil, Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def literalSequence05(): Bool =
        let input = fromString("abc");
        let actual = literalSequence('b' :: 'a' :: Nil, input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // String                                                             //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def string01(): Bool =
        let input = noInput();
        let actual = string("", input);
        let expected = pr2((Nil, Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def string02(): Bool =
        let input = fromString("abc");
        let actual = string("", input);
        let expected = pr2((Nil, 'a' :: 'b' :: 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def string03(): Bool =
        let input = fromString("abc");
        let actual = string("ab", input);
        let expected = pr2(('a' :: 'b' :: Nil, 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def string04(): Bool =
        let input = fromString("abc");
        let actual = string("abc", input);
        let expected = pr2(('a' :: 'b' :: 'c' :: Nil, Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def string05(): Bool =
        let input = fromString("abc");
        let actual = string("ba", input);
        let expected = pr(Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Return                                                             //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def return01(): Bool =
        let input = noInput();
        let actual = input |> (succeed(Nil) `return` "");
        let expected = pr(("", Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def return02(): Bool =
        let input = noInput();
        let actual = input |> (fail `return` "");
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def return03(): Bool =
        let input = fromString("");
        let actual = input |> (string("") `return` "");
        let expected = pr(("", Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def return04(): Bool =
        let input = fromString("");
        let actual = input |> (string("") `return` 'a');
        let expected = pr(('a', Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def return05(): Bool =
        let input = fromString("");
        let actual = input |> (string("") `return` 1);
        let expected = pr((1, Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def return06(): Bool =
        let input = fromString("abc");
        let actual = input |> (string("") `return` 1);
        let expected = pr((1, 'a' :: 'b' :: 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def return07(): Bool =
        let input = fromString("abc");
        let actual = input |> (string("ab") `return` 1);
        let expected = pr((1, 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def return08(): Bool =
        let input = fromString("abc");
        let actual = input |> (string("ab") `return` "");
        let expected = pr(("", 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Stringify                                                          //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def stringify01(): Bool =
        let input = fromString("");
        let actual = stringify(input);
        let expected = "";
        Assert.eq(expected, actual)

    @Test
    def stringify02(): Bool =
        let input = fromString("aaab");
        let actual = stringify(input);
        let expected = "aaab";
        Assert.eq(expected, actual)

    @Test
    def stringify03(): Bool =
        let input = fromString("12345");
        let actual = number(input)
            |> DelayList.head
            |> Option.map(fst >> stringify);
        let expected = Some("12345");
        Assert.eq(expected, actual)

    ////////////////////////////////////////////////////////////////////////
    // Nibble                                                             //
    ////////////////////////////////////////////////////////////////////////

    @Test
    def nibble01(): Bool =
        let input = fromString("");
        let actual: ParseResult[Char, Char] = input |> (nibble(fail));
        let expected = pr(Nil);
        Assert.eq(expected, actual)

    @Test
    def nibble02(): Bool =
        let input = fromString("");
        let actual = input |> (nibble(succeed("")));
        let expected = pr(("", Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def nibble03(): Bool =
        let input = fromString("");                     // `succeed` yields whatever
        let actual = input |> (nibble(succeed("   "))); // it's passed regardless of
        let expected = pr(("   ", Nil) :: Nil);         // any function that processes the input
        Assert.eq(expected, actual)

    @Test
    def nibble04(): Bool =
        let input = fromString("");                        // `succeed` yields whatever
        let actual = input |> (nibble(succeed("  abc "))); // it's passed regardless of
        let expected = pr(("  abc ", Nil) :: Nil);         // any function that processes the input
        Assert.eq(expected, actual)

    @Test
    def nibble05(): Bool =
        let input = fromString("    aaa      ");
        let actual = input |> (nibble(string("aaa")));
        let expected = pr2(
            ('a' :: 'a' :: 'a' :: Nil, Nil) ::
            ('a' :: 'a' :: 'a' :: Nil, ' ' :: Nil) ::
            ('a' :: 'a' :: 'a' :: Nil, ' ' :: ' ' :: Nil) ::
            ('a' :: 'a' :: 'a' :: Nil, ' ' :: ' ' :: ' ' :: Nil) ::
            ('a' :: 'a' :: 'a' :: Nil, ' ' :: ' ' :: ' ' :: ' ' :: Nil) ::
            ('a' :: 'a' :: 'a' :: Nil, ' ' :: ' ' :: ' ' :: ' ' :: ' ' :: Nil) ::
            ('a' :: 'a' :: 'a' :: Nil, ' ' :: ' ' :: ' ' :: ' ' :: ' ' :: ' ' :: Nil) ::
            Nil);
        Assert.eq(expected, actual)

    @Test
    def nibble06(): Bool =
        let input = fromString("    aaaabc");
        let actual = input |> (nibble(string("aaa")));
        let expected = pr2(('a' :: 'a' :: 'a' :: Nil, 'a' :: 'b' :: 'c' :: Nil) :: Nil);
        Assert.eq(expected, actual)

    @Test
    def nibble07(): Bool =
        let input = fromString("    Hello   world!");
        let actual = input |> (nibble(string("Hello")))
            |> DelayList.head
            |> Option.map(match (a, b) -> (stringify(a), stringify(b)));
        let expected = Some("Hello", "world!");
        Assert.eq(expected, actual)

}
