namespace Wrebuf.Tests


open System
open Xunit
open FParsec
open Wirebuf.Ast
open Wirebuf.Protobuf.Parser

[<AutoOpen>]
module XUnitExtensions =
    type Assert with
        static member Parse<'t>(parser,  expected : 't,  text) =
            match run parser text with
            | Success (result, _, _) -> Assert.Equal(expected, result)
            | Failure (err, _, _) -> Assert.True(false, err)
        static member NotParse<'t>(parser : Parser<'t, unit>,  text) =
            match run parser text with
            | Success (result, _, _) -> Assert.True(false, sprintf "Parsed in: %A"  result)
            | Failure (err, _, _) -> Assert.True(true)

type ParserTests() =
    [<Fact>]
    member  __.``Letter gold path``() =
        let test = "A"
        Assert.Parse(pletter, test.[0], test)
    [<Fact>]
    member  __.``Letter invalid chars``() =
        let test = "Б"
        Assert.NotParse(pletter, test)

    [<Fact>]
    member  __.``Decimal digit gold path``() =
        let test = "1"
        Assert.Parse(pdecimalDigit, test.[0], test)
    [<Fact>]
    member  __.``Decimal digit invalid chars``() =
        let test = "БEF"
        Assert.NotParse(pdecimalDigit, test)


    [<Fact>]
    member  __.``Octal digit gold path``() =
        let test = "1"
        Assert.Parse(poctalDigit, test.[0], test)
    [<Fact>]
    member  __.``Octal digit invalid chars``() =
        let test = "9"
        Assert.NotParse(poctalDigit, test)

    [<Fact>]
    member  __.``Hexadecimal digit gold path``() =
        let test = "A"
        Assert.Parse(phexDigit, test.[0], test)

    [<Fact>]
    member  __.``Hexadecimal digit invalid chars``() =
        let test = "БEF"
        Assert.NotParse(phexDigit, test)

    [<Fact>]
    member  __.``Identifier gold path``() =
        let test = "Abc12_def"
        Assert.Parse(pident, Identifier.CreateUnchecked test, test)
    [<Fact>]
    member  __.``Identifier invalid chars``() =
        let test = "БEF"
        Assert.NotParse(pident, test)

    [<Fact>]
    member  __.``Qualified identifier gold path``() =
        let test =
            QualifiedIdentifier.CreateUnchecked [
                Identifier.CreateUnchecked "Abc123"
                Identifier.CreateUnchecked "def"
            ]
        Assert.Parse(pfullIdent, test, test.ToString())
    [<Fact>]
    member  __.``Qualified identifier invalid chars``() =
        let test = "asdncfg..asdf"
        Assert.NotParse(pfullIdent, test)

    [<Fact>]
    member __.``Parse decimal literal``() =
        let test = "129345"
        Assert.Parse(pdecimalLit, DecLit test, test)

    [<Fact>]
    member __.``Parse octal literal``() =
        let test = "012345"
        Assert.Parse(poctalLit, OctalLit test, test)

    [<Fact>]
    member __.``Parse hex literal``() =
        let test = "0x12345"
        Assert.Parse(phexLit, HexLit "12345", test)

    [<Fact>]
    member __.``Parse int literal``() =
        Assert.Parse(pintLit, DecLit "9898", "9898")
        Assert.Parse(pintLit, OctalLit "01234", "01234")
        Assert.Parse(pintLit, HexLit "12A34", "0x12A34")


    [<Fact>]
    member __.``Parse float literal``() =
        Assert.Parse(pfloatLit, Nan , "nan")
        Assert.Parse(pfloatLit, Inf , "inf")
        Assert.Parse(pfloatLit, FloatLit("22.", None)  , "22.")
        Assert.Parse(pfloatLit, FloatLit("22.22", None)  , "22.22")
        Assert.Parse(pfloatLit, FloatLit("22.22", { Sign = Plus; Digits = "22" } |> Some)  , "22.22E22")
        Assert.Parse(pfloatLit, FloatLit("22.22", { Sign = Minus; Digits = "22" } |> Some)  , "22.22E-22")
        Assert.Parse(pfloatLit, FloatLit("22", { Sign = Plus; Digits = "22" } |> Some)  , "22E22")
        Assert.Parse(pfloatLit, FloatLit(".22", { Sign = Plus; Digits = "22" } |> Some)  , ".22E22")

    [<Fact>]
    member __.``Parse string literal``() =
        Assert.Parse(pstrLit, { Quote = DoubleQuote; Value ="ABCDEF\n" }, "\"ABCDEF\\n\"")
        Assert.Parse(pstrLit, { Quote = SingleQuote; Value ="ABCDEF\n" }, "'ABCDEF\\n'")
        Assert.Parse(pstrLit, { Quote = SingleQuote; Value ="ABCDEF\n" }, "'ABCDEF\\x0A'")

    [<Fact>]
    member __.``Parse constant``() =
        Assert.Parse(pconstant, { Quote = DoubleQuote; Value ="ABCDEF\n" } |> StrConst, "\"ABCDEF\\n\"")
        Assert.Parse(pconstant, (Minus, "1234" |> DecLit) |> IntConst, "- 1234")
        Assert.Parse(pconstant, (Minus, ("1234.56", None) |> FloatLit) |> FloatConst, "- 1234.56")
        Assert.Parse(pconstant, true |> BoolConst, "true")
        let test =
                    QualifiedIdentifier.CreateUnchecked [
                        Identifier.CreateUnchecked "Abc123"
                        Identifier.CreateUnchecked "def"
                    ]
        Assert.Parse(pconstant, test |> IdentConst, test.ToString())



