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

    // Helper functions

    def noInput(): List[String] = Nil

    def input(s: String): List[String] = s :: Nil

    ////////////////////////////////////////////////////////////////////////
    // Succeed                                                            //
    ////////////////////////////////////////////////////////////////////////

    @test
    def succeed01(): Bool =
        let input = noInput();
        let actual = Parser.succeed("", input);
        let expected = ("", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def succeed02(): Bool =
        let input = noInput();
        let actual = Parser.succeed("a", input);
        let expected = ("a", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def succeed03(): Bool =
        let input = input("abc");
        let actual = Parser.succeed("a", input);
        let expected = ("a", input) :: Nil;
        Assert.eq(expected, actual)

    @test
    def succeed04(): Bool =
        let input = "abc" :: "123" :: "hey" :: Nil;
        let actual = Parser.succeed("success", input);
        let expected = ("success", input) :: Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Fail                                                               //
    ////////////////////////////////////////////////////////////////////////

    @test
    def fail01(): Bool =
        let input = noInput();
        let actual: ParseResult[String, String] = Parser.fail(input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def fail03(): Bool =
        let input = input("abc");
        let actual: ParseResult[String, String] = Parser.fail(input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def fail04(): Bool =
        let input = "abc" :: "123" :: "hey" :: Nil;
        let actual: ParseResult[String, String] = Parser.fail(input);
        let expected = Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Satisfy                                                            //
    ////////////////////////////////////////////////////////////////////////

    @test
    def satisfy01(): Bool =
        let input = noInput();
        let actual = Parser.satisfy(a -> a == "a", input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def satisfy02(): Bool =
        let input = input("a");
        let actual = Parser.satisfy(a -> a == "a", input);
        let expected = ("a", Nil) :: Nil;
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
        let actual = Parser.literal("a", input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def literal02(): Bool =
        let input = input("a");
        let actual = Parser.literal("a", input);
        let expected = ("a", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def literal03(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = Parser.literal("a", input);
        let expected = ("a", "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def literal04(): Bool =
        let input = "b" :: "a" :: "c" :: Nil;
        let actual = Parser.literal("a", input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def literal05(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = Parser.literal("b", input);
        let expected = Nil;
        Assert.eq(expected, actual)


    ////////////////////////////////////////////////////////////////////////
    // Append                                                             //
    ////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        let input = noInput();
        let actual = Parser.append(Parser.literal("a"), Parser.literal("b"), input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def append02(): Bool =
        let input = input("a");
        let actual = Parser.append(Parser.literal("a"), Parser.literal("b"), input);
        let expected = ("a", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append03(): Bool =
        let input = "a" :: "b" :: Nil;
        let actual = Parser.append(Parser.literal("a"), Parser.literal("b"), input);
        let expected = ("a", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append04(): Bool =
        let input = "a" :: "b" :: Nil;
        let actual = Parser.append(Parser.literal("a"), Parser.literal("a"), input);
        let expected = ("a", "b" :: Nil) :: ("a", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append05(): Bool =
        let input = "b" :: "a" :: Nil;
        let actual = Parser.append(Parser.literal("a"), Parser.literal("b"), input);
        let expected = ("b", "a" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append06(): Bool =
        let input = "b" :: "b" :: Nil;
        let actual = Parser.append(Parser.literal("a"), Parser.literal("b"), input);
        let expected = ("b", "b" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append07(): Bool =
        let input = "b" :: "c" :: Nil;
        let actual = Parser.append(Parser.literal("b"), Parser.literal("b"), input);
        let expected = ("b", "c" :: Nil) :: ("b", "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append08(): Bool =
        let input = input("c");
        let actual = Parser.append(Parser.literal("a"), Parser.literal("b"), input);
        let expected = Nil;
        Assert.eq(expected, actual)

    @test
    def append09(): Bool =
        let input = noInput();
        let actual = Parser.append(Parser.succeed("1"), Parser.succeed("2"), input);
        let expected = ("1", Nil) :: ("2", Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append10(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = Parser.append(Parser.succeed("1"), Parser.succeed("2"), input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: ("2", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append11(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = Parser.append(Parser.succeed("1"), Parser.fail, input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append12(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual = Parser.append(Parser.fail, Parser.succeed("1"), input);
        let expected = ("1", "a" :: "b" :: "c" :: Nil) :: Nil;
        Assert.eq(expected, actual)

    @test
    def append13(): Bool =
        let input = "a" :: "b" :: "c" :: Nil;
        let actual: ParseResult[String, String] = Parser.append(Parser.fail, Parser.fail, input);
        let expected = Nil;
        Assert.eq(expected, actual)

}
