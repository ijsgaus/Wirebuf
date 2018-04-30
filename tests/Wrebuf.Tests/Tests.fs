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
        Assert.Parse(letter, test.[0], test)
    [<Fact>]
    member  __.``Letter invalid chars``() =
        let test = "Б"
        Assert.NotParse(letter, test)

    [<Fact>]
    member  __.``Decimal digit gold path``() =
        let test = "1"
        Assert.Parse(decimalDigit, test.[0], test)
    [<Fact>]
    member  __.``Decimal digit invalid chars``() =
        let test = "БEF"
        Assert.NotParse(decimalDigit, test)


    [<Fact>]
    member  __.``Octal digit gold path``() =
        let test = "1"
        Assert.Parse(octalDigit, test.[0], test)
    [<Fact>]
    member  __.``Octal digit invalid chars``() =
        let test = "9"
        Assert.NotParse(octalDigit, test)

    [<Fact>]
    member  __.``Hexadecimal digit gold path``() =
        let test = "A"
        Assert.Parse(hexDigit, test.[0], test)

    [<Fact>]
    member  __.``Hexadecimal digit invalid chars``() =
        let test = "БEF"
        Assert.NotParse(hexDigit, test)

    [<Fact>]
    member  __.``Identifier gold path``() =
        let test = "Abc12_def"
        Assert.Parse(ident, Identifier.CreateUnchecked test, test)
    [<Fact>]
    member  __.``Identifier invalid chars``() =
        let test = "БEF"
        Assert.NotParse(ident, test)

    [<Fact>]
    member  __.``Qualified identifier gold path``() =
        let test =
            QualifiedIdentifier.CreateUnchecked [
                Identifier.CreateUnchecked "Abc123"
                Identifier.CreateUnchecked "def"
            ]
        Assert.Parse(fullIdent, test, test.ToString())
    [<Fact>]
    member  __.``Qualified identifier invalid chars``() =
        let test = "asdncfg..asdf"
        Assert.NotParse(fullIdent, test)

    [<Fact>]
    member __.``Parse decimal literal``() =
        let test = "129345"
        Assert.Parse(decimalLit, DecLit test, test)

    [<Fact>]
    member __.``Parse octal literal``() =
        let test = "012345"
        Assert.Parse(octalLit, OctalLit test, test)

    [<Fact>]
    member __.``Parse hex literal``() =
        let test = "0x12345"
        Assert.Parse(hexLit, HexLit "12345", test)

    [<Fact>]
    member __.``Parse int literal``() =
        Assert.Parse(intLit, DecLit "9898", "9898")
        Assert.Parse(intLit, OctalLit "01234", "01234")
        Assert.Parse(intLit, HexLit "12A34", "0x12A34")


    [<Fact>]
    member __.``Parse float literal``() =
        Assert.Parse(floatLit, Nan , "nan")
        Assert.Parse(floatLit, Inf , "inf")
        Assert.Parse(floatLit, FloatLit("22.", None)  , "22.")
        Assert.Parse(floatLit, FloatLit("22.22", None)  , "22.22")
        Assert.Parse(floatLit, FloatLit("22.22", { Sign = Plus; Digits = "22" } |> Some)  , "22.22E22")
        Assert.Parse(floatLit, FloatLit("22.22", { Sign = Minus; Digits = "22" } |> Some)  , "22.22E-22")
        Assert.Parse(floatLit, FloatLit("22", { Sign = Plus; Digits = "22" } |> Some)  , "22E22")
        Assert.Parse(floatLit, FloatLit(".22", { Sign = Plus; Digits = "22" } |> Some)  , ".22E22")




