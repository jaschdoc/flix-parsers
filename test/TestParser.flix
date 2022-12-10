namespace TestParser {

    use Assert.eq;

    // Helper functions

    def noInput(): List[String] = Nil

    def input(s: String): List[String] = s :: Nil

    ////////////////////////////////////
    // Succeed                        //
    ////////////////////////////////////

    @test
    def succeed01(): Bool =
        let input = noInput();
        let actual = Parser.succeed("", input);
        let expected = ("", Nil) :: Nil;
        eq(expected, actual)

    @test
    def succeed02(): Bool =
        let input = noInput();
        let actual = Parser.succeed("a", input);
        let expected = ("a", Nil) :: Nil;
        eq(expected, actual)

    @test
    def succeed03(): Bool =
        let input = input("abc");
        let actual = Parser.succeed("a", input);
        let expected = ("a", input) :: Nil;
        eq(expected, actual)

    @test
    def succeed04(): Bool =
        let input = "abc" :: "123" :: "hey" :: Nil;
        let actual = Parser.succeed("success", input);
        let expected = ("success", input) :: Nil;
        eq(expected, actual)

}
