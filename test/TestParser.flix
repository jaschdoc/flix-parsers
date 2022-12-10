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
        let input: List[String] = noInput();
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

}
