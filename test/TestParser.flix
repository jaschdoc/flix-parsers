namespace TestParser {

    use Assert.eq;

    ////////////////////////////////////
    /// Succeed                      ///
    ////////////////////////////////////

    @test
    def succeed01(): Bool =
        let actual = ("" |> Parser.succeed)(Nil: List[String]);
        let expected = ("", Nil) :: Nil;
        eq(expected, actual)

}
