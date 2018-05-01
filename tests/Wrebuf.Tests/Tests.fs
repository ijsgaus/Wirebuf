namespace Wrebuf.Tests


open System
open Xunit
open FParsec
open Wirebuf.Ast
open Wirebuf.Protobuf.Ast
open Wirebuf.Protobuf.Parser

[<AutoOpen>]
module XUnitExtensions =
    type Assert with
        static member Parse<'t>(parser,  expected : 't,  text) =
                    match run parser text with
                    | Success (result, _, _) -> Assert.Equal(expected, result)
                    | Failure (err, _, _) -> Assert.True(false, err)
        static member ParseNode<'t when 't :> IAstNode>(parser,  expected : 't,  text) =
            match run parser text with
            | Success (result, _, _) -> Assert.Equal(expected, result); Assert.Equal(text, result.ToSource())
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
        Assert.ParseNode(pident, Ident.CreateUnchecked test, test)
    [<Fact>]
    member  __.``Identifier invalid chars``() =
        let test = "БEF"
        Assert.NotParse(pident, test)

    [<Fact>]
    member  __.``Qualified identifier gold path``() =
        let test =
            FullIdent.CreateUnchecked [
                Ident.CreateUnchecked "Abc123"
                Ident.CreateUnchecked "def"
            ]
        Assert.ParseNode(pfullIdent, test, (test :> IAstNode).ToSource())
    [<Fact>]
    member  __.``Qualified identifier invalid chars``() =
        let test = "asdncfg..asdf"
        Assert.NotParse(pfullIdent, test)

    [<Fact>]
    member __.``Parse decimal literal``() =
        let test = "129345"
        Assert.ParseNode(pdecimalLit, DecLit test, test)

    [<Fact>]
    member __.``Parse octal literal``() =
        let test = "012345"
        Assert.ParseNode(poctalLit, OctalLit test, test)

    [<Fact>]
    member __.``Parse hex literal``() =
        let test = "0x12345"
        Assert.ParseNode(phexLit, HexLit "12345", test)

    [<Fact>]
    member __.``Parse int literal``() =
        Assert.ParseNode(pintLit, DecLit "9898", "9898")
        Assert.ParseNode(pintLit, OctalLit "01234", "01234")
        Assert.ParseNode(pintLit, HexLit "12A34", "0x12A34")


    [<Fact>]
    member __.``Parse float literal``() =
        Assert.ParseNode(pfloatLit, Nan , "nan")
        Assert.ParseNode(pfloatLit, Inf , "inf")
        Assert.ParseNode(pfloatLit, FloatLit("22.", None)  , "22.")
        Assert.ParseNode(pfloatLit, FloatLit("22.22", None)  , "22.22")
        Assert.ParseNode(pfloatLit, FloatLit("22.22", { IsUpper = true; Sign = None; Digits = "22" } |> Some)  , "22.22E22")
        Assert.ParseNode(pfloatLit, FloatLit("22.22", { IsUpper = false; Sign = Some Minus; Digits = "22" } |> Some)  , "22.22e-22")
        Assert.ParseNode(pfloatLit, FloatLit("22", { IsUpper = true; Sign = None; Digits = "22" } |> Some)  , "22E22")
        Assert.ParseNode(pfloatLit, FloatLit(".22", { IsUpper = false; Sign = Some Plus; Digits = "22" } |> Some)  , ".22e+22")
(*
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

*)

